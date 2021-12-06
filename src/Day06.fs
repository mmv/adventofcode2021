module Day06

// We don't need to treat the fishes individually since their only
// distinguishing behavior is the day counter... We can just handle
// day counts instead.

// Needed to change arithmetic to bigint for part2, but also works
// for part1.

let updateCounts cs =
    Map.fold
        (fun s k v ->
            match (k,v) with
            | 0,v ->
                s
                |> Map.change 8 (Option.defaultValue 0I >> ((+) v) >> Some)
                |> Map.change 6 (Option.defaultValue 0I >> ((+) v) >> Some)
            | n,v ->
                s
                |> Map.change (n-1) (Option.defaultValue 0I >> ((+) v) >> Some)
        )
        Map.empty
        cs

let solveX days =
    Utils.readLines 6
    |> Array.head
    |> (fun s -> s.Split(',') |> Seq.map int)
    |> Seq.countBy id
    |> Seq.map (fun (d,v) -> (d, bigint v))
    |> Map.ofSeq
    |> Seq.foldBack (fun _ s -> updateCounts s) (seq { 1..days })
    |> Map.values
    |> Seq.sum
    |> string

let solve1() = solveX 80

let solve2() = solveX 256