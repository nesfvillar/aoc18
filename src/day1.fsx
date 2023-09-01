module Day1

let input =
    "inputs/day1.txt"
    |> System.IO.File.ReadAllLines
    |> (fun lines -> Seq.map int lines)

let Part1 (input: seq<int>) = Seq.sum input

let repeatSeq items = seq { while true do yield! items }

let Part2 (input: seq<int>) =
    let input = repeatSeq input
    let items = Seq.scan (fun acc elem -> acc + elem) 0 input
    let seen = items |> Seq.scan (fun (acc: int Set) elem -> acc.Add elem) Set.empty

    Seq.zip items seen
    |> Seq.find (fun (value, seen) -> seen.Contains value)
    |> fst