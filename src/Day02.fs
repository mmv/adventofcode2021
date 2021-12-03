module Day02

let parseLine (line: string) =
    let parts = line.Split(" ")
    (parts.[0], int(parts.[1]))

let updatePos (x: int, y: int) (dir: string, n: int) =
    match dir with
    | "forward" -> (x+n, y  )
    | "up"      -> (x,   y-n)
    | "down"    -> (x,   y+n)
    | _ -> failwith "unexpected"

let solve1() =
    Utils.readLines 2
    |> Seq.map parseLine
    |> Seq.fold updatePos (0,0)
    |> (fun (a,b) -> a*b)
    |> string

let updatePos' (x: int, y: int, aim: int) (dir: string, n: int) =
    match dir with
    | "forward" -> (x+n, y+aim*n, aim)
    | "up"      -> (x,   y,       aim-n)
    | "down"    -> (x,   y,       aim+n)
    | _ -> failwith "unexpected"

let solve2() =
    Utils.readLines 2
    |> Seq.map parseLine
    |> Seq.fold updatePos' (0,0,0)
    |> (fun (a,b,_) -> a*b)
    |> string