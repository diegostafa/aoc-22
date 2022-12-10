open System.IO

module Part1 =
    let valueOf c =
        match c with
        | _ when c >= 'a' -> int c - int 'a' + 1
        | _ -> int c - int 'A' + 27

    let totalValue cs =
        Seq.fold (fun state c -> state + (valueOf c)) 0 cs

    let foldMatch state curr =
        match curr with
        | (x, y) ->
            List.append state (Set.toList (Set.intersect (Set.ofList (Seq.toList x)) (Set.ofList (Seq.toList y))))

    let splitInHalf (str: string) =
        (str.[.. (str.Length / 2 - 1)], str.[str.Length / 2 ..])

    let solve =
        printfn
            "Total value is: %i"
            (totalValue (
                File.ReadAllText("input").Split("\n")
                |> Seq.map splitInHalf
                |> Seq.fold foldMatch []
            ))

module Part2 =
    let valueOf c =
        match c with
        | _ when c >= 'a' -> int c - int 'a' + 1
        | _ -> int c - int 'A' + 27

    let totalValue cs =
        Seq.fold (fun state c -> state + (valueOf c)) 0 cs

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

    let solve =
        printfn
            "Result: %i"
            (totalValue (
                File.ReadAllText("input").Split("\n")
                |> Array.toList
                |> List.chunkBySize 3
                |> List.fold foldMatch []
            ))
