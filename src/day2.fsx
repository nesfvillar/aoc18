module Day2

let input =
    "inputs/day2.txt"
    |> System.IO.File.ReadAllLines
    |> seq

let getCharacterCounts id =
    id
    |> Seq.countBy (fun x -> x)


let validId (id: string) (times: int) =
    id
    |> getCharacterCounts
    |> Seq.countBy (fun (c, n) -> n)
    |> Seq.exists (fun (n, _) -> n = times)

let Part1 input =
    let valid2 = fun id -> validId id 2
    let valid3 = fun id -> validId id 3

    let numValid2 = input |> Seq.filter valid2 |> Seq.length
    let numValid3 = input |> Seq.filter valid3 |> Seq.length

    numValid2 * numValid3
