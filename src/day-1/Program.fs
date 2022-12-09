open System.IO

module Part1 =
    let sumCals (strBlock:string) =
        List.fold (fun acc cal -> acc + (int cal)) 0 (strBlock.Split "\n" |> Array.toList)

    let (main: unit) =
        printfn
            "Result: %i"
            (File.ReadAllText("input").Split("\n\n")
             |> Array.toList
             |> List.map sumCals
             |> List.max)

module Part2 =
    let sumCals (strBlock:string) =
        List.fold (fun acc cal -> acc + (int cal)) 0 (strBlock.Split "\n" |> Array.toList)

    let (main: unit) =
        printfn
            "Result: %i"
            (File.ReadAllText("input").Split("\n\n")
             |> Array.toList
             |> List.map sumCals
             |> List.sortDescending
             |> List.take 3
             |> List.sum)
