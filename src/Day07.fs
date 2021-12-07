module Day07

let fuelScore xs p =
    xs
    |> Seq.sumBy (((-) p) >> abs)

let fuelScore' xs p =
    xs
    |> Seq.map (((-) p) >> abs)
    |> Seq.sumBy (fun d -> seq { 1..d } |> Seq.sum )

let solveX fuelScoreFn =
    let positions =
        Utils.readLines 7
        |> Array.head
        |> fun s -> s.Split(',') |> Array.map int
    
    seq { (Seq.min positions)..(Seq.max positions) }
    |> Seq.minBy (fuelScoreFn positions)
    |> fuelScoreFn positions
    |> string

let solve1() = solveX fuelScore
let solve2() = solveX fuelScore'