open System.IO

let isContained ((a, b), (c, d)) = a >= c && b <= d || a <= c && b >= d

let isOverlapping ((a, b), (c, d)) =
    a >= c && a <= d || b >= c && b <= d || c >= a && c <= b || d >= a && d <= b

let toPairs (line: string) =
    let tokens = line.Split(",")
    let sx = tokens[ 0 ].Split("-")
    let dx = tokens[ 1 ].Split("-")
    ((int sx[0], int sx[1]), (int dx[0], int dx[1]))

let foldContained state curr =
    if isContained curr then state + 1 else state

let silver =
    printfn
        "Result: %A"
        (File.ReadAllText("input").Split("\n")
         |> Seq.map toPairs
         |> Seq.fold foldContained 0)

let gold =
    printfn
        "Result: %A"
        (File.ReadAllText("input").Split("\n")
         |> Seq.map toPairs
         |> Seq.fold foldContained 0)
