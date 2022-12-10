open System.IO

module Part1 =
    let isContained ((a, b), (c, d)) = a >= c && b <= d || a <= c && b >= d

    let toPairs (line: string) =
        let tokens = line.Split(",")
        let sx = tokens[ 0 ].Split("-")
        let dx = tokens[ 1 ].Split("-")
        ((int sx[0], int sx[1]), (int dx[0], int dx[1]))

    let foldContained state curr =
        if isContained curr then state + 1 else state

    let solve =
        printfn
            "Total value is: %i"
            (File.ReadAllText("input").Split("\n")
             |> Seq.map toPairs
             |> Seq.fold foldContained 0)

module Part2 =
    let isOverlapping ((a, b), (c, d)) =
        a >= c && a <= d || b >= c && b <= d || c >= a && c <= b || d >= a && d <= b

    let toPairs (line: string) =
        let tokens = line.Split(",")
        let sx = tokens[ 0 ].Split("-")
        let dx = tokens[ 1 ].Split("-")
        ((int sx[0], int sx[1]), (int dx[0], int dx[1]))

    let foldContained state curr =
        if isOverlapping curr then state + 1 else state

    let solve =
        printfn
            "Total value is: %i"
            (File.ReadAllText("input").Split("\n")
             |> Seq.map toPairs
             |> Seq.fold foldContained 0)
