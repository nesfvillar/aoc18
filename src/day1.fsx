let input =
    "inputs/day1.txt"
    |> System.IO.File.ReadAllLines
    |> (fun lines -> Seq.map int lines)

let Part1 (input: seq<int>) = Seq.sum input

printfn "Part 1 solution: %d" (Part1 input)