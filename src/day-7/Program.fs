open System.IO

module Part1 =
    let remFileNames (line: string) =
        match line.Split(" ") |> Seq.toList with
        | "$" :: _ -> line
        | "dir" :: _ -> line
        | any :: _ -> any
        | _ -> line

    let rec foldSize (input: string list) (dict: Map<string, int>) =
        match input with
        | [] -> []
        | x :: xs ->
            if x = "$ cd .." then
                foldSize xs dict
            else
                match List.splitAt (List.findIndex (fun str -> str = "$ ls") input) input with
                | (files, b) ->
                    let dirSize =
                        List.fold
                            (fun state (l: string) ->
                                match l.Split(" ") |> Array.toList with
                                | "dir" :: d :: _ -> state + dict[d]
                                | any :: _ -> state + (any |> int)
                                | _ -> state)
                            0
                            files

                    [ (dirSize) ]
                    @ (foldSize (List.removeManyAt 0 2 b) (Map.add (b[ 1 ].Split(" ")[2]) dirSize dict))

    let (main: unit) =
        let input =
            List.map remFileNames (List.rev (File.ReadAllText("input").Split("\n") |> Array.toList))

        printfn "Result: %i" (List.sum (List.filter (fun sz -> sz <= 100000) (foldSize input Map.empty)))


module Part2 =
    let remFileNames (line: string) =
        match line.Split(" ") |> Seq.toList with
        | "$" :: _ -> line
        | "dir" :: _ -> line
        | any :: _ -> any
        | _ -> line

    let rec foldSize (input: string list) (dict: Map<string, int>) =
        match input with
        | [] -> []
        | x :: xs ->
            if x = "$ cd .." then
                foldSize xs dict
            else
                match List.splitAt (List.findIndex (fun str -> str = "$ ls") input) input with
                | (files, b) ->
                    let dirSize =
                        List.fold
                            (fun state (l: string) ->
                                match l.Split(" ") |> Array.toList with
                                | "dir" :: d :: _ -> state + dict[d]
                                | any :: _ -> state + (any |> int)
                                | _ -> state)
                            0
                            files

                    [ (dirSize) ]
                    @ (foldSize (List.removeManyAt 0 2 b) (Map.add (b[ 1 ].Split(" ")[2]) dirSize dict))

    let (main: unit) =
        let input =
            List.map remFileNames (List.rev (File.ReadAllText("input").Split("\n") |> Array.toList))

        let rootSz = List.fold (fun state (line:string) ->
                                match line.Split(" ") |> Seq.toList with
                                | "$" :: _ -> state
                                | "dir" :: _ -> state
                                | any :: _ -> state + (any |> int)
                                | _ -> state
                            ) 0 input

        let unusedSpace = 70000000 - rootSz
        let neededSpace = 30000000 - unusedSpace
        let validDirs = List.filter (fun sz -> sz >= neededSpace) (foldSize input Map.empty)
        let smallest = List.min validDirs

        printfn "Result: %i" smallest
