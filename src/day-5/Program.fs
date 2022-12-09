open System.IO

module Part1 =
    let foldChunk (state: (((char list) array) * int)) (chunk: char list) =
        match state with
        | (arr, i) ->
            match chunk[1] with
            | ' ' -> (arr, i + 1)
            | _ -> ((Array.updateAt i ([ chunk[1] ] @ arr[i]) arr), i + 1)


    let foldCrane (craneArr: (char list) array) (row: (char list) list) =
        match (List.fold foldChunk (craneArr, 1) row) with
        | (arr, _) -> arr


    let printResult (craneArr: (char list) array) =
        for i in [ 1 .. craneArr.Length - 1 ] do
            printf "%c" (List.last craneArr[i])

    let execMove (craneArr: (char list) array) qnt fromIndex destIndex =
        let tmp = List.splitAt (craneArr[fromIndex].Length - qnt) craneArr[fromIndex]

        match tmp with
        | (a, b) ->
            let removedFrom = Array.updateAt fromIndex a craneArr

            let (res: (char list) array) =
                Array.updateAt destIndex (removedFrom[destIndex] @ (List.rev b)) removedFrom

            res

    let (main: unit) =
        let parts = File.ReadAllText("input").Split("\n\n")
        let rawMoves = parts[ 1 ].Split("\n") |> Array.toList
        let moves = List.map (fun (str: string) -> str.Split(" ") |> Array.toList) rawMoves
        let rawCranes = (parts[ 0 ].Split("\n") |> Array.toList)

        let chunkedCranes =
            List.map (fun l -> List.chunkBySize 4 (Seq.toList l)) (List.removeAt (rawCranes.Length - 1) rawCranes)

        let cranes =
            List.fold foldCrane (Array.create (chunkedCranes.Head.Length + 1) []) chunkedCranes

        printResult (
            List.fold
                (fun arr (move: string list) -> execMove arr (int move[1]) (int move[3]) (int move[5]))
                cranes
                moves
        )

module Part2 =
    let foldChunk (state: (((char list) array) * int)) (chunk: char list) =
        match state with
        | (arr, i) ->
            match chunk[1] with
            | ' ' -> (arr, i + 1)
            | _ -> ((Array.updateAt i ([ chunk[1] ] @ arr[i]) arr), i + 1)


    let foldCrane (craneArr: (char list) array) (row: (char list) list) =
        match (List.fold foldChunk (craneArr, 1) row) with
        | (arr, _) -> arr


    let printResult (craneArr: (char list) array) =
        for i in [ 1 .. craneArr.Length - 1 ] do
            printf "%c" (List.last craneArr[i])

    let execMove (craneArr: (char list) array) qnt fromIndex destIndex =
        let tmp = List.splitAt (craneArr[fromIndex].Length - qnt) craneArr[fromIndex]

        match tmp with
        | (a, b) ->
            let removedFrom = Array.updateAt fromIndex a craneArr

            let (res: (char list) array) =
                Array.updateAt destIndex (removedFrom[destIndex] @ b) removedFrom

            res

    let (main: unit) =
        let parts = File.ReadAllText("input").Split("\n\n")
        let rawMoves = parts[ 1 ].Split("\n") |> Array.toList
        let moves = List.map (fun (str: string) -> str.Split(" ") |> Array.toList) rawMoves
        let rawCranes = (parts[ 0 ].Split("\n") |> Array.toList)

        let chunkedCranes =
            List.map (fun l -> List.chunkBySize 4 (Seq.toList l)) (List.removeAt (rawCranes.Length - 1) rawCranes)

        let cranes =
            List.fold foldCrane (Array.create (chunkedCranes.Head.Length + 1) []) chunkedCranes

        printResult (
            List.fold
                (fun arr (move: string list) -> execMove arr (int move[1]) (int move[3]) (int move[5]))
                cranes
                moves
        )
