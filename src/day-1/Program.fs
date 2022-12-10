open System.IO

module Part1 =
    let sumCals (strBlock:string) =
        Seq.fold (fun acc cal -> acc + (int cal)) 0 (strBlock.Split "\n")

    let (main: unit) =
        printfn
            "Result: %i"
            (File.ReadAllText("input").Split("\n\n")
             |> Seq.map sumCals
             |> Seq.max)

module Part2 =
    let sumCals (strBlock:string) =
        Seq.fold (fun acc cal -> acc + (int cal)) 0 (strBlock.Split "\n")

    let (main: unit) =
        printfn
            "Result: %i"
            (File.ReadAllText("input").Split("\n\n")
             |> Seq.map sumCals
             |> Seq.sortDescending
             |> Seq.take 3
             |> Seq.sum)
