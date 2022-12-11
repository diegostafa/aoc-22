open System.IO

let sumCals (strBlock:string) =
    Seq.fold (fun acc cal -> acc + (int cal)) 0 (strBlock.Split "\n")

let silver =
    printfn
        "Result: %A"
        (File.ReadAllText("input").Split("\n\n")
            |> Seq.map sumCals
            |> Seq.max)

let gold =
    printfn
        "Result: %A"
        (File.ReadAllText("input").Split("\n\n")
            |> Seq.map sumCals
            |> Seq.sortDescending
            |> Seq.take 3
            |> Seq.sum)
