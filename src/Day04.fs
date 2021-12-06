module Day04

let bsize = 5

let readBoard (lines: string seq) =
    lines
    |> Seq.mapi (
        fun x ns ->
            ns.Trim().Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
            |> Seq.mapi (fun y n -> ((x,y), int n)))
    |> Seq.concat
    |> Map.ofSeq

type BoardTracker = {
    board: Map<(int * int),int>
    npos: Map<int,(int * int)>
    marks: Map<int*int, bool>
    hmarks: Map<int,int>
    vmarks: Map<int,int>
}

let boardToTracker (board: Map<(int*int),int>) =
    {
        board = board
        npos = board |> Map.toSeq |> Seq.map (fun (k,v) -> (v,k)) |> Map.ofSeq
        marks = Seq.init bsize (fun x -> Seq.init bsize (fun y -> ((x,y),false))) |> Seq.concat |> Map.ofSeq
        hmarks = Seq.init bsize (fun x -> (x,bsize)) |> Map.ofSeq
        vmarks = Seq.init bsize (fun y -> (y,bsize)) |> Map.ofSeq
    }

let updateTracker n tracker =
    let npos = tracker.npos |> Map.tryFind n
    match npos with
    | Some(x,y) ->
        {
            tracker with 
                hmarks = tracker.hmarks |> Map.add x (tracker.hmarks.[x] - 1)
                vmarks = tracker.vmarks |> Map.add y (tracker.vmarks.[y] - 1)
                marks = tracker.marks   |> Map.add (x,y) true
        }
    | None -> tracker

let checkWin tracker =
    tracker.hmarks |> Map.values
    |> Seq.append (tracker.vmarks |> Map.values)
    |> Seq.contains 0

let boardScore x t =
    let unmarked =
        t.board
        |> Map.values
        |> Seq.filter (fun v ->
            Map.find 
                (Map.find v t.npos)
                t.marks
            |> not)
        |> Utils.tap (printfn "%A")
    
    unmarked |> Seq.fold (+) 0 |> (*) x


let solveX() =
    let input = Utils.readLines 4
    let xs = (input |> Seq.head).Split(',') |> Seq.map int
    let ts =
        input
        |> Seq.skip 2
        |> Utils.batchSplit
        |> Seq.map (readBoard >> boardToTracker)

    xs
    |> Seq.scan
        (fun (ts,_) x ->
            (
                ts |> Seq.map (updateTracker x),
                x
            )
        )
        (ts, 0)


let solve1() =
    solveX()
    |> Seq.choose (fun (ts,x) ->
        let winner = ts |> Seq.tryFind checkWin
        winner
        |> Option.map (boardScore x)
    )
    |> Seq.head
    |> string

let solve2() =
    solveX()
    // |> Seq.last
    // |> printfn "%A"
    |> Seq.collect (fun (ts,x) ->
        ts
        |> Seq.filter checkWin
        |> Seq.map (fun t -> (t, x))
    )
    // |> Utils.tap (fun (t,_) -> printfn "%A" t.board)
    |> Seq.distinctBy (fun (t,_) -> t.board)
    // |> Utils.tap (printfn "%A")
    |> Seq.last
    |> (fun (t,x) -> boardScore x t)
    |> string
    // ""