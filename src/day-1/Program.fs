open System.IO

module Part1 =
    let lines (strBlock: string) = strBlock.Split "\n" |> Array.toList

    let sumCals strBlock =
        List.fold (fun state str -> state + (int str)) 0 (lines strBlock)

    let (main: unit) =
        printfn
            "Result: %i"
            (File.ReadAllText("input").Split("\n\n")
             |> Array.toList
             |> List.map sumCals
             |> List.max)

module Part2 =
    let lines (strBlock: string) = strBlock.Split "\n" |> Array.toList

    let sumCals strBlock =
        List.fold (fun state str -> state + (int str)) 0 (lines strBlock)

    let (main: unit) =
        printfn
            "Result: %i"
            (File.ReadAllText("input").Split("\n\n")
             |> Array.toList
             |> List.map sumCals
             |> List.sortDescending
             |> List.take 3
             |> List.sum)
