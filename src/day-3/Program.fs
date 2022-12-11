open System.IO

let valueOf c =
    match c with
    | _ when c >= 'a' -> int c - int 'a' + 1
    | _ -> int c - int 'A' + 27

let foldMatchSilver state curr =
    match curr with
    | (x, y) -> List.append state (Set.toList (Set.intersect (Set.ofList (Seq.toList x)) (Set.ofList (Seq.toList y))))

let foldMatchGold state curr =
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

let silver =
    printfn
        "Total value is: %i"
        (File.ReadAllText("input").Split("\n")
         |> Seq.map (fun str -> (str.[.. (str.Length / 2 - 1)], str.[str.Length / 2 ..]))
         |> Seq.fold foldMatchSilver []
         |> Seq.fold (fun state c -> state + (valueOf c)) 0)

let gold =
    printfn
        "Result: %i"
        (File.ReadAllText("input").Split("\n")
         |> Array.toList
         |> List.chunkBySize 3
         |> List.fold foldMatchGold []
         |> Seq.fold (fun state c -> state + (valueOf c)) 0)
