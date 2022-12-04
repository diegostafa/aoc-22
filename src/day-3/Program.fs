open System
open System.IO
open System.Collections.Generic

module Part1 =
    let valueOf c =
        match c with
        | _ when c >= 'a' -> int c - int 'a' + 1
        | _ -> int c - int 'A' + 27

    let totalValue cs =
        List.fold (fun state c -> state + (valueOf c)) 0 cs

    let foldMatch state curr =
        match curr with
        | (x, y) ->
            List.append state (Set.toList (Set.intersect (Set.ofList (Seq.toList x)) (Set.ofList (Seq.toList y))))

    let splitInHalf (str: string) =
        (str.[.. (str.Length / 2 - 1)], str.[str.Length / 2 ..])

    let (main: unit) =
        let lines = (File.ReadAllText("input").Split("\n") |> Array.toList)
        let splits = (List.map splitInHalf lines)
        let matchedChars = List.fold foldMatch [] splits
        printfn "Total value is: %i" (totalValue matchedChars)

module Part2 =
    let valueOf c =
        match c with
        | _ when c >= 'a' -> int c - int 'a' + 1
        | _ -> int c - int 'A' + 27

    let totalValue cs =
        List.fold (fun state c -> state + (valueOf c)) 0 cs

    let foldMatch state curr =
        match curr with
        | [ x; y; z ] ->
            List.append
                state
                (Set.toList (
                    (Set.intersect
                        (Set.ofList (Seq.toList x))
                        (Set.intersect (Set.ofList (Seq.toList y)) (Set.ofList (Seq.toList z))))
                ))
        | _ -> state

    let (main: unit) =
        let lines = (File.ReadAllText("input").Split("\n") |> Array.toList)
        let grouped = List.chunkBySize 3 lines
        let matchedChars = List.fold foldMatch [] (grouped)
        printfn "Total value is: %i" (totalValue matchedChars)
