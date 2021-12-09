module Day09

/// points adjacent to the given point
let adj (map: int[][]) (x,y) =
    let sx = map |> Array.head |> Array.length
    let sy = map |> Array.length
    seq {
        x-1, y
        x+1, y
        x, y-1
        x, y+1
    }
    |> Seq.filter (fun (x,y) -> 0 <= x && x < sx && 0 <= y && y < sy)

let adjv map p = adj map p |> Seq.map (fun (x,y) -> (map.[y].[x]))

let solve1() =
    let map =
        Utils.readLines 9
        |> Array.map (fun line -> line |> Seq.map (fun c -> (int c)-(int '0'))  |> Seq.toArray)
    map
    |> Seq.mapi (fun y -> Seq.mapi (fun x v -> (x,y),v))
    |> Seq.concat
    |> Seq.filter (fun ((x,y),v) -> adjv map (x,y) |> Seq.forall ((<) v))
    |> Seq.sumBy (snd >> ((+) 1))
    |> string


let basinFiller (map: int[][]) p =
    /// recursively add new points to the basin by checking in all the
    /// valid points to visit and then recursively visiting the adjacent
    /// points to the ones added
    let rec fill visited next =
        let added = next |> Set.filter (fun (x,y) -> (map.[y].[x] < 9))
        if Seq.isEmpty added
        then visited
        else
            let visited' =
                Set.union
                    visited
                    added
            let next' =
                added
                |> Seq.collect (adj map)
                |> Seq.filter (fun p -> not(Set.contains p visited'))
                |> set
            fill visited' next'
    fill (Set.empty) (Set.singleton p)

let solve2 () =
    let map =
        Utils.readLines 9
        |> Array.map (fun line -> line |> Seq.map (fun c -> (int c)-(int '0'))  |> Seq.toArray)
    let allPositions =
        map
        |> Seq.mapi (fun y -> Seq.mapi (fun x _ -> (x,y)))
        |> Seq.concat
    
    // fold the points keeping a tracker of the ones that were already
    // added so we can skip those when we find them later on
    let basins, _ =
        Seq.fold
            (fun (basins, seen) p ->
                if Set.contains p seen
                then (basins, seen)
                else
                    let basin = basinFiller map p
                    if Set.isEmpty basin
                    then (basins, seen |> Set.add p)
                    else (basin::basins, seen |> Set.union basin)
            )
            (List.empty, Set.empty) allPositions
    
    basins
    |> List.map (Set.count)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.reduce (*)
    |> string