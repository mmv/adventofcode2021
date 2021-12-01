

let solvers =  [|
    Day01.solve1;   Day01.solve2
|]

printfn "%s" ((Array.last solvers)())
