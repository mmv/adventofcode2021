module Day05

let parseLine (line: string) =
    let parseCoord (c: string) =
        let (x,y) = Utils.pairSplit "," c
        (int x, int y)

    let (s,e) = Utils.pairSplit " -> " line
    (parseCoord s, parseCoord e)

let linePosWithoutDiag (s: int*int) (e: int*int) =
    match s,e with
    | (x1,y1),(x2,y2) when x1 = x2 ->
        seq { for y in (min y1 y2)..(max y1 y2) do yield (x1,y) }
    | (x1,y1),(x2,y2) when y1 = y2 ->
        seq { for x in (min x1 x2)..(max x1 x2) do yield (x,y1) }
    | _ -> Seq.empty

let linePosWithDiag ((x1,y1): int*int) ((x2,y2): int*int) =
    let increment = (compare x2 x1),(compare y2 y1)
    let padd (a,b) (c,d) = (a+c,b+d)
    let pmul (a,b) x = (a*x, b*x)
    Seq.initInfinite (pmul increment)
    |> Seq.scan (fun _ i -> padd (x1,y1) i) (x1,y1)
    |> Seq.takeWhile ((<>) (x2,y2))
    |> Seq.append (seq { (x2,y2) })
    |> Seq.tail

let solveX linePositionReader =
    let map =
        Utils.readLines 5
        |> Seq.map parseLine
        |> Seq.collect (fun (s,e) -> linePositionReader s e)
        |> Seq.fold (fun m p ->
                m |> Map.change p (fun v -> Option.defaultValue 0 v |> ((+) 1) |> Some)
            )
            Map.empty

    map
    |> Map.values
    |> Seq.filter ((<) 1)
    |> Seq.length
    |> string


let solve1() =
    solveX linePosWithoutDiag

let solve2() =
    solveX linePosWithDiag