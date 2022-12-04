open System.IO

module Part1 =
    let isContained ((a, b), (c, d)) = a >= c && b <= d || a <= c && b >= d

    let toPairs (line: string) =
        let tokens = line.Split(",") |> Array.toList
        let sx = tokens[ 0 ].Split("-") |> Array.toList
        let dx = tokens[ 1 ].Split("-") |> Array.toList
        ((sx[0] |> int, sx[1] |> int), (dx[0] |> int, dx[1] |> int))

    let foldContained state curr =
        if isContained curr then state + 1 else state

    let (main: unit) =
        let lines = (File.ReadAllText("input").Split("\n") |> Array.toList)
        let pairs = List.map toPairs lines
        let totCont = List.fold foldContained 0 pairs
        printfn "Total value is: %i" totCont

module Part2 =
    let isOverlapping ((a, b), (c, d)) =
        a >= c && a <= d || b >= c && b <= d || c >= a && c <= b || d >= a && d <= b

    let toPairs (line: string) =
        let tokens = line.Split(",") |> Array.toList
        let sx = tokens[ 0 ].Split("-") |> Array.toList
        let dx = tokens[ 1 ].Split("-") |> Array.toList
        ((sx[0] |> int, sx[1] |> int), (dx[0] |> int, dx[1] |> int))

    let foldContained state curr =
        if isOverlapping curr then state + 1 else state

    let (main: unit) =
        let lines = (File.ReadAllText("input").Split("\n") |> Array.toList)
        let pairs = List.map toPairs lines
        let totCont = List.fold foldContained 0 pairs
        printfn "Total value is: %i" totCont
