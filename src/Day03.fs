module Day03

let biggestIn (x: Map<char, int>) =
    Map.toSeq x
    |> Seq.maxBy (snd)      // for the biggest value
    |> fst                  // get the key
    
let lowestIn (x: Map<char, int>) = 
    Map.toSeq x
    |> Seq.minBy (snd)      // for the smallest value
    |> fst                  // get the key

let bitFold (xs: char seq) =
    Seq.fold (fun s x -> (s<<<1) + (if x = '0' then 0 else 1)) 0 xs

let solve1() =
    // transpose the array so we get a list of columns
    // count the bit values in each column

    let bcounts = 
        Utils.readLines 3
        |> Array.map (fun s -> s.ToCharArray())
        |> Array.transpose
        |> Array.map (Seq.countBy id >> Map.ofSeq)
    
    let gamma   = bcounts |> Seq.map biggestIn |> bitFold
    let epsilon = bcounts |> Seq.map lowestIn  |> bitFold

    gamma * epsilon
    |> string


let solve2() =
    let byRows = 
        Utils.readLines 3
        |> Seq.map (fun s -> s.ToCharArray())
        |> Seq.toList

    // xs: the bit sequences to filter
    // fs: the function to filter for (lower than / bigger than)
    // e:  the value to use when there's a tie
    // p:  the bit position being evaluated 
    let rec sieve (xs: char[] seq) (fs: int -> int -> bool) e p =

        // recursive function? stop case: having a single candidate
        if 1 = Seq.length xs
        then
            Seq.head xs
        else
            let ccounts =
                xs
                |> Seq.map (fun w -> w.[p])
                |> Seq.countBy id
                |> Map.ofSeq
            
            // the bit character to match for will depend on the `fs` filter
            // but default to 'e' if the values are tied
            let bChar =
                if ccounts.['0'] = ccounts.['1']
                then e 
                elif fs ccounts.['0'] ccounts.['1']
                then '0'
                else '1'

            sieve
                (Seq.filter (fun x -> x.[p] = bChar) xs)
                fs
                e
                (p+1)
    
    let oxigen = sieve byRows (>) '1' 0 |> bitFold
    let co2 = sieve byRows (<) '0' 0 |> bitFold

    oxigen * co2
    |> string
    