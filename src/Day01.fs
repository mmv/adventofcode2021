module Day01

let solve1 () =
    Utils.readLines 1
    |> Seq.map int
    |> Seq.pairwise
    |> Seq.sumBy (fun (a,b) -> if b > a then 1 else 0)
    |> string

let solve2() =
    let x0 = Utils.readLines 1 |> Seq.map int
    let x1 = Seq.tail x0
    let x2 = Seq.tail x1
    let xs = Seq.zip3 x0 x1 x2
    xs
    |> Seq.map (fun (a,b,c) -> a + b + c)
    |> Seq.pairwise
    |> Seq.sumBy (fun (a,b) -> if b > a then 1 else 0)
    |> string
