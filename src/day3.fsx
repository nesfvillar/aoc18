type Claim =
    { Id: int
      Location: int * int
      Size: int * int }

let parse (i: int) (i': int) (claim: string) = (claim.Substring i).Remove(i' - i)

let parseBetween (s: string) (s': string) (claim: string) =
    let i = claim.IndexOf s + 1
    let i' = claim.IndexOf s'
    parse i i' claim

let parseId (claim: string) = parseBetween "#" "@" claim |> int

let parseLocation (claim: string) =
    let result = parseBetween "@" ":" claim
    let result' = result.Split "," |> Seq.map int
    result' |> Seq.head, result' |> Seq.tail |> Seq.head

let parseSize (claim: string) =
    let i = claim.IndexOf ":" + 1
    let result = claim.Substring i
    let result' = result.Split "x" |> Seq.map int
    result' |> Seq.head, result' |> Seq.tail |> Seq.head

let parseClaim (input: string) : Claim =
    { Id = parseId input
      Location = parseLocation input
      Size = parseSize input }

let input =
    "inputs/day3.txt"
    |> System.IO.File.ReadAllLines
    |> Seq.map (fun s -> s |> String.filter (fun c -> c <> ' '))
    |> Seq.map parseClaim
