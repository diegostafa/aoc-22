open System.IO

module Part1 =
    let (toInt: string -> int) = fun str -> str |> int
    let (lines: string -> string list) = fun strBlock ->  strBlock.Split "\n" |> Array.toList
    let (foldSum: int -> string -> int) = fun state str -> state + (toInt str)
    let (sumCals: string -> int) = fun strBlock -> List.fold foldSum 0 (lines strBlock)
    
    let (main: unit) = 
        let txt: string = File.ReadAllText("input")
        let allCals: string list = txt.Split("\n\n") |> Array.toList
        let totCalsByElf: int list = List.map sumCals allCals
        printfn "max is: %i" (List.max totCalsByElf)

module Part2 =
    let (toInt: string -> int) = fun str -> str |> int
    let (lines: string -> string list) = fun strBlock ->  strBlock.Split "\n" |> Array.toList
    let (foldSum: int -> string -> int) = fun state str -> state + (toInt str)
    let (sumCals: string -> int) = fun strBlock -> List.fold foldSum 0 (lines strBlock)
    
    let (main: unit) = 
        let txt: string = File.ReadAllText("input")
        let allCals: string list = txt.Split("\n\n") |> Array.toList
        let totCalsByElf: int list = List.map sumCals allCals
        let sortedTotCalsByElf: int list = List.sortDescending totCalsByElf
        printfn "Total top 3 is: %i" (List.sum (List.take 3 sortedTotCalsByElf))