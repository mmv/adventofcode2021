module Day08

let digit2segment = function
    | 0 -> "abcefg"
    | 1 -> "cf"
    | 2 -> "acdeg"
    | 3 -> "acdfg"
    | 4 -> "bcdf"
    | 5 -> "abdfg"
    | 6 -> "abdefg"
    | 7 -> "acf"
    | 8 -> "abcdefg"
    | 9 -> "abcdfg"
    | _ -> failwith "out of range"

let solve1() =

    let uniqSz =
        [| 0..9 |]
        |> Seq.countBy (digit2segment >> String.length)
        |> Seq.filter (snd >> ((=) 1))
        |> Seq.map fst
        |> set

    Utils.readLines 8
    |> Seq.collect (fun line -> line.Split('|').[1].Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
                                |> Seq.map (fun d -> d.Length))
    |> Seq.filter (uniqSz.Contains)
    |> Seq.length
    |> string


let findMapping (samples: string seq) =
    // take a set of possibilities (a char can be any other char)
    // and reduce the possibilities through a bunch of steps until
    // we get to a 1:1 mapping

    let vocabulary = "abcdefg" |> set
    let possible = vocabulary |> Seq.map (fun x -> (x,vocabulary)) |> Map.ofSeq
    let segCombos = [| 0..9 |] |> Array.map digit2segment

    // match by segment size
    let pass1 =
        Seq.fold (fun ps (s: string) ->
            let matchingSize = segCombos |> Seq.filter (fun seg -> seg.Length = s.Length)
            printfn "Matching size %d: %A" (s.Length) (matchingSize |> Seq.toArray)
            Seq.fold (fun ps c ->
                ps |> Map.add c (Set.intersect (ps.[c]) (Set.unionMany (matchingSize |> Seq.map set)))
            ) ps s
        ) possible samples
    // match by frequency
    let sfreq = Seq.concat samples |> Seq.countBy id
    let cfreq = Seq.concat segCombos |> Seq.countBy id
    let pass2 =
        Seq.fold (fun ps (c,f) ->
            ps |> Map.add c (cfreq |> Seq.filter (snd >> ((=) f)) |> Seq.map fst |> set |> Set.intersect ps.[c])
        ) pass1 sfreq
    // remove assigned
    let pass3 =
        Map.fold (fun ps c cs ->
            if Set.count cs = 1 then
                ps |> Map.map (fun k cs' -> if k = c then cs' else Set.difference cs' cs)
            else
                ps
            ) pass2 pass2
    // all the possibility sets must have a single candidate at this time
    pass3 |> Map.map (fun _ v -> v |> Seq.exactlyOne)

let solve2() =
    let segment2digit = [| 0..9 |] |> Array.map (fun d -> digit2segment d, d) |> Map.ofSeq
    let joinDigit = Seq.fold (fun s x -> s * 10 + x) 0

    Utils.readLines 8
    |> Seq.map (
        Utils.pairSplit " | "
        >> (fun (sample, code) -> findMapping (sample.Split(" ")), code.Split(" "))
        >> (fun (map, code) ->
            code
            |> Seq.map(fun cs ->
                Map.find
                    (cs
                        |> Seq.map (fun c -> map.[c])
                        |> Seq.sort
                        |> Seq.toArray |> System.String
                    )
                    segment2digit
            ))
        >> joinDigit
    )
    |> Seq.sum
    |> string