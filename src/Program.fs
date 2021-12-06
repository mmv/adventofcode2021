

let solvers =  [|
    Day01.solve1;   Day01.solve2
    Day02.solve1;   Day02.solve2
    Day03.solve1;   Day03.solve2
    Day04.solve1;   Day04.solve2
    Day05.solve1;   Day05.solve2
    Day06.solve1;   Day06.solve2
|]

printfn "%s" ((Array.last solvers)())
