open System.IO

module Part1 =
    let toPair (line: string) =
        let tokens = line.Split(" ") |> Array.toList
        (tokens[0], tokens[1])

    let (moveValue: (string * string) -> int) =
        fun (x, y) ->
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


    let (main: unit) =
        let text = File.ReadAllText("input")
        let lines: string list = text.Split("\n") |> Array.toList
        let moves: (string * string) list = List.map toPair lines
        let movesValue: (int list) = List.map moveValue moves
        printfn "Total value is: %i" (List.sum movesValue)

module Part2 =
    let toPair (line: string) =
        let tokens = line.Split(" ") |> Array.toList
        (tokens[0], tokens[1])

    let (moveValue: (string * string) -> int) =
        fun (x, y) ->
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

    let (main: unit) =
        let text = File.ReadAllText("input")
        let lines: string list = text.Split("\n") |> Array.toList
        let moves: (string * string) list = List.map toPair lines
        let movesValue: (int list) = List.map moveValue moves
        printfn "Total value is: %i" (List.sum movesValue)
