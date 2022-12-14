open System.IO

type Packet =
    | Data of int
    | Packet of Packet list

let toPair (str: string) =
    let a = str.Split("\n")
    (a[0], a[1])

let endBracket (input: string) =
    let mutable op = 0
    let mutable pos = -1

    for i in 0 .. input.Length - 1 do
        if input[i] = '[' then
            op <- op + 1
        elif input[i] = ']' then
            op <- op - 1

            if op = 0 && pos = -1 then
                pos <- i

    pos

let endDigit (input: string) =
    let marker = ','
    let mutable pos = 0

    while pos < input.Length && not (input[pos] = marker) do
        pos <- pos + 1

    pos

let rec toPacket (input: string) =
    if input = "[]" then
        Packet []
    else
        let inp = input.Substring(1, input.Length - 2)
        let mutable pks = []
        let mutable i = 0
        let mutable skip = 0

        while i < inp.Length do
            skip <- 0

            if int inp[i] >= 48 && int inp[i] <= 57 then
                skip <- endDigit (inp.Substring(i))
                pks <- pks @ [ Data(int (inp.Substring(i, skip))) ]

            elif inp[i] = '[' then
                skip <- endBracket (inp.Substring(i))
                pks <- pks @ [ toPacket (inp.Substring(i, skip + 1)) ]

            i <- i + skip + 1

        Packet pks

let rec cmpPackets p1 p2 =
    match p1, p2 with
    | Data a, Data b ->
        match a < b with
        | true -> -1
        | false -> if a = b then 0 else 1
    | Data _, Packet _ -> cmpPackets (Packet [ p1 ]) p2
    | Packet _, Data _ -> cmpPackets p1 (Packet [ p2 ])
    | Packet a, Packet b ->
        match a, b with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        | x :: xs, y :: ys ->
            match (cmpPackets x y) with
            | a when not (a = 0) -> a
            | _ -> cmpPackets (Packet xs) (Packet ys)

let silver =
    printfn
        "Result: %A"
        (File.ReadAllText("sinput").Split("\n\n")
         |> Array.toList
         |> List.map toPair
         |> List.map (fun (x, y) -> (toPacket x, toPacket y))
         |> List.map (fun (x, y) -> cmpPackets x y)
         |> List.indexed
         |> List.fold (fun acc (i, cmp) -> if cmp = -1 then acc + i + 1 else acc) 0)

let gold =
    let decs = [ Packet([ Packet([ Data(2) ]) ]); Packet([ Packet([ Data(6) ]) ]) ]

    let inp =
        File.ReadAllText("input").Replace("\n\n", "\n").Split("\n")
        |> Array.toList
        |> List.map toPacket
        |> List.append decs
        |> List.sortWith cmpPackets

    printfn
        "Result: %A"
        (((List.findIndex (fun p -> (cmpPackets p decs[0]) = 0) inp) + 1)
         * ((List.findIndex (fun p -> (cmpPackets p decs[1]) = 0) inp) + 1))
