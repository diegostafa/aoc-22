open System.IO

type Operand =
    | Num of int64
    | Var

type Operation =
    | Add of (Operand * Operand)
    | Sub of (Operand * Operand)
    | Div of (Operand * Operand)
    | Mul of (Operand * Operand)

type Monkey =
    { id: int
      mutable items: int64 list
      op: Operation
      divBy: int64
      throwTo: (int * int) }

let parseOp (op1: string) (operator: string) (op2: string) =
    let sx =
        match op1 with
        | "old" -> Var
        | n -> Num(int64 n)

    let dx =
        match op2 with
        | "old" -> Var
        | n -> Num(int64 n)

    match operator with
    | "+" -> Add(sx, dx)
    | "-" -> Sub(sx, dx)
    | "*" -> Mul(sx, dx)
    | "/" -> Div(sx, dx)
    | _ -> Add(sx, dx)

let parseMonke (yaml: string) =
    let lines =
        yaml.Split("\n")
        |> Array.map (fun l -> l.Split(":") |> Array.map (fun (p: string) -> p.Trim()))

    let id = (lines[0][0]).Split(" ")[1] |> int

    let items =
        (lines[1][1]).Split(",") |> Seq.map (fun i -> i.Trim() |> int64) |> Seq.toList

    let operands = (lines[2][1]).Split(" ")
    let op = parseOp operands[2] operands[3] operands[4]
    let divBy = (lines[3][1]).Split(" ")[2] |> int64

    let throwTo =
        (((lines[4][1]).Split(" ")[3]) |> int, ((lines[5][1]).Split(" ")[3]) |> int)

    { id = id
      items = items
      op = op
      divBy = divBy
      throwTo = throwTo }

let replace exp var =
    match exp with
    | Num(a) -> a
    | Var -> var

let evalOp exp var =
    match exp with
    | Add(a, b) -> (replace a var) + (replace b var)
    | Sub(a, b) -> (replace a var) - (replace b var)
    | Mul(a, b) -> (replace a var) * (replace b var)
    | Div(a, b) -> (replace a var) / (replace b var)

let part1 =
    let ms = File.ReadAllText("input").Split("\n\n") |> Array.map parseMonke
    let mutable im = Array.fold (fun map m -> Map.add m.id 0 map) Map.empty ms
    let intz = int64 0

    for _ in 1..20 do
        for m in ms do
            for item in m.items do
                let newItem = (evalOp m.op item) / (int64 3)

                let dest =
                    match newItem % m.divBy with
                    | a when a = intz -> fst m.throwTo
                    | _ -> snd m.throwTo

                ms[dest].items <- List.append ms[dest].items [ newItem ]
                im <- (Map.add m.id (im[m.id] + 1) im)

            m.items <- []

    printfn "Result %A" (im.Values |> Seq.sortDescending |> Seq.take 2 |> Seq.fold (fun p v -> p * v) 1)


let part2 =
    let ms = File.ReadAllText("input").Split("\n\n") |> Array.map parseMonke

    let lcm =
        Array.map (fun m -> m.divBy) ms |> Array.fold (fun prod d -> prod * d) (int64 1)

    let mutable im = Array.fold (fun map m -> Map.add m.id 0 map) Map.empty ms
    let intz = int64 0

    for _ in 1..10000 do
        for m in ms do
            for item in m.items do
                let newItem = (evalOp m.op item) % lcm

                let dest =
                    match newItem % m.divBy with
                    | a when a = intz -> fst m.throwTo
                    | _ -> snd m.throwTo

                ms[dest].items <- List.append ms[dest].items [ newItem ]
                im <- (Map.add m.id (im[m.id] + 1) im)

            m.items <- []

    printfn
        "Result %A"
        (im.Values
         |> Seq.sortDescending
         |> Seq.take 2
         |> Seq.map bigint
         |> Seq.fold (fun p v -> p * v) (bigint 1))
