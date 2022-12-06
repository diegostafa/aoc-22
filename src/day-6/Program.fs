open System.IO

module Part1 =

    let rec scrollWindow (stream: char list) currPos windowSize =
        match stream with
        | [] -> currPos
        | x :: xs ->
            let buffer = (Seq.truncate windowSize stream) |> Seq.toList

            if ((List.distinct buffer).Length = buffer.Length) then
                currPos + windowSize
            else
                scrollWindow xs (currPos + 1) windowSize

    let (main: unit) =
        let charStream = File.ReadAllText("input") |> Seq.toList
        let markerPos = scrollWindow charStream 0

        printfn "Result: %i" (markerPos 4)

module Part2 =

    let rec scrollWindow (stream: char list) currPos windowSize =
        match stream with
        | [] -> currPos
        | x :: xs ->
            let buffer = (Seq.truncate windowSize stream) |> Seq.toList

            if ((List.distinct buffer).Length = buffer.Length) then
                currPos + windowSize
            else
                scrollWindow xs (currPos + 1) windowSize

    let (main: unit) =
        let charStream = File.ReadAllText("input") |> Seq.toList
        let markerPos = scrollWindow charStream 0

        printfn "Result: %i" (markerPos 14)
