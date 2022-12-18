open System.IO

let input =
    let remFileNames (line: string) =
        match line.Split(" ") |> Seq.toList with
        | "$" :: _ -> line
        | "dir" :: _ -> line
        | any :: _ -> any
        | _ -> line

    File.ReadAllText("input").Split("\n")
    |> Seq.rev
    |> Seq.map remFileNames
    |> Seq.toList

let rec foldSize (input: string list) (dict: Map<string, int>) =
    match input with
    | [] -> []
    | x :: xs when x = "$ cd .." -> foldSize xs dict
    | x :: xs ->
        let (files, b) =  input |> List.splitAt (List.findIndex (fun str -> str = "$ ls") input) with
        let dirSize =
            List.fold
                (fun state (l: string) ->
                    match l.Split(" ") |> Array.toList with
                    | "dir" :: d :: _ -> state + dict[d]
                    | any :: _ -> state + (int any)
                    | _ -> state)
                0
                files

        [ (dirSize) ]
        @ (foldSize (List.removeManyAt 0 2 b) (Map.add (b[ 1 ].Split(" ")[2]) dirSize dict))

let silver =
    printfn "Result: %A" (foldSize input Map.empty |> List.filter (fun sz -> sz <= 100000) |> List.sum)

let gold =
    let rootSz =
        Seq.fold
            (fun state (line: string) ->
                match line.Split(" ") |> Seq.toList with
                | "$" :: _ -> state
                | "dir" :: _ -> state
                | any :: _ -> state + (int any)
                | _ -> state)
            0
            input

    printfn
        "Result: %A"
        (foldSize input Map.empty
         |> List.filter (fun sz -> sz >= 30000000 - 70000000 - rootSz)
         |> List.min)
