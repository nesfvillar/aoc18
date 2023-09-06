module Day1

let input =
    "inputs/day1.txt"
    |> System.IO.File.ReadAllLines
    |> (fun lines -> Seq.map int lines)

let Part1 (input: int seq) = Seq.sum input

let repeatSeq items =
    seq {
        while true do
            yield! items
    }

let Part2 (input: int seq) =
    let items = repeatSeq input |> Seq.scan (fun acc elem -> acc + elem) 0
    let seen = items |> Seq.scan (fun (acc: int Set) elem -> acc.Add elem) Set.empty

    Seq.zip items seen |> Seq.find (fun (elem, seen) -> seen.Contains elem) |> fst
