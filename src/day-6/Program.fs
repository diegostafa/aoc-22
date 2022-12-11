open System.IO

let rec scrollWindow currPos windowSize (stream: char list) =
    match stream with
    | [] -> currPos
    | x :: xs ->
        let buffer = (Seq.truncate windowSize stream) |> Seq.toList

        if ((List.distinct buffer).Length = buffer.Length) then
            currPos + windowSize
        else
            scrollWindow (currPos + 1) windowSize xs

let silver =
    printfn "Result: %A" (File.ReadAllText("input") |> Seq.toList |> scrollWindow 0 4)

let gold =
    printfn "Result: %A" (File.ReadAllText("input") |> Seq.toList |> scrollWindow 0 14)
