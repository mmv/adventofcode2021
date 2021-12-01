module Utils

open System.IO

let readLines (day: int) =
    let file = $"Inputs/{day}"
    File.ReadAllLines file

let tap f xs = seq {
    for x in xs do
        f x
        yield x
}