open System.IO

let toPair (line: string) =
    let tokens = line.Split(" ")
    (tokens[0], tokens[1])

let moveValueSilver (x, y) =
    match (x, y) with
    | ("A", "X") -> 4
    | ("A", "Y") -> 8
    | ("A", "Z") -> 3
    | ("B", "X") -> 1
    | ("B", "Y") -> 5
    | ("B", "Z") -> 9
    | ("C", "X") -> 7
    | ("C", "Y") -> 2
    | ("C", "Z") -> 6
    | (_, _) -> 0

let moveValueGold (x, y) =
    match (x, y) with
    | ("A", "X") -> 3
    | ("A", "Y") -> 4
    | ("A", "Z") -> 8
    | ("B", "X") -> 1
    | ("B", "Y") -> 5
    | ("B", "Z") -> 9
    | ("C", "X") -> 2
    | ("C", "Y") -> 6
    | ("C", "Z") -> 7
    | (_, _) -> 0

let silver =
    printfn
        "Result: %A"
        (File.ReadAllText("input").Split("\n")
         |> Seq.map toPair
         |> Seq.map moveValueSilver
         |> Seq.sum)

let gold =
    printfn
        "Result: %A"
        (File.ReadAllText("input").Split("\n")
         |> Seq.map toPair
         |> Seq.map moveValueGold
         |> Seq.sum)
