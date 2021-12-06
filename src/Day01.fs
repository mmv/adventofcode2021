module Day01

let solve1 () =
    Utils.readLines 1
    |> Seq.map int
    |> Seq.pairwise
    |> Seq.sumBy (fun (a,b) -> if b > a then 1 else 0)
    |> string

let solve2() =
    Utils.readLines 1
    |> Seq.map int
    |> Seq.windowed 3
    |> Seq.map Array.sum
    |> Seq.pairwise
    |> Seq.sumBy (fun (a,b) -> if b > a then 1 else 0)
    |> string