open System.IO

module Part1 =
    let (main: unit) =
        let input = File.ReadAllText("input").Split("\n") |> Seq.toList
        printfn ""