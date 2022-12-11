open System.IO

let printResult (craneArr: (char list) array) =
    for i in [ 1 .. craneArr.Length - 1 ] do
        printf "%c" (List.last craneArr[i])

let foldChunk (state: (((char list) array) * int)) (chunk: char list) =
    match state with
    | (arr, i) ->
        match chunk[1] with
        | ' ' -> (arr, i + 1)
        | _ -> ((Array.updateAt i ([ chunk[1] ] @ arr[i]) arr), i + 1)

let foldCrane (craneArr: (char list) array) (row: (char list) list) =
    match (List.fold foldChunk (craneArr, 1) row) with
    | (arr, _) -> arr

let execMoveSilver (craneArr: (char list) array) qnt fromIndex destIndex =
    let tmp = List.splitAt (craneArr[fromIndex].Length - qnt) craneArr[fromIndex]

    match tmp with
    | (a, b) ->
        let removedFrom = Array.updateAt fromIndex a craneArr

        let res =
            Array.updateAt destIndex (removedFrom[destIndex] @ (List.rev b)) removedFrom

        res

let execMoveGold (craneArr: (char list) array) qnt fromIndex destIndex =
    let tmp = List.splitAt (craneArr[fromIndex].Length - qnt) craneArr[fromIndex]

    match tmp with
    | (a, b) ->
        let removedFrom = Array.updateAt fromIndex a craneArr

        let res =
            Array.updateAt destIndex (removedFrom[destIndex] @ b) removedFrom

        res

let silver =
    let parts = File.ReadAllText("input").Split("\n\n")
    let rawMoves = parts[ 1 ].Split("\n") |> Array.toList
    let rawCranes = (parts[ 0 ].Split("\n") |> Array.toList)
    let moves = List.map (fun (str: string) -> str.Split(" ") |> Array.toList) rawMoves

    let chunkedCranes =
        List.map (fun l -> List.chunkBySize 4 (Seq.toList l)) (List.removeAt (rawCranes.Length - 1) rawCranes)

    let cranes =
        List.fold foldCrane (Array.create (chunkedCranes.Head.Length + 1) []) chunkedCranes

    printResult (
        List.fold
            (fun arr (move: string list) -> execMoveSilver arr (int move[1]) (int move[3]) (int move[5]))
            cranes
            moves
    )

let gold =
    let parts = File.ReadAllText("input").Split("\n\n")
    let rawMoves = parts[ 1 ].Split("\n") |> Array.toList
    let rawCranes = (parts[ 0 ].Split("\n") |> Array.toList)
    let moves = List.map (fun (str: string) -> str.Split(" ") |> Array.toList) rawMoves

    let chunkedCranes =
        List.map (fun l -> List.chunkBySize 4 (Seq.toList l)) (List.removeAt (rawCranes.Length - 1) rawCranes)

    let cranes =
        List.fold foldCrane (Array.create (chunkedCranes.Head.Length + 1) []) chunkedCranes

    printResult (
        List.fold
            (fun arr (move: string list) -> execMoveGold arr (int move[1]) (int move[3]) (int move[5]))
            cranes
            moves
    )
