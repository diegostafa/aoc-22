open System.IO

type Sensor = { x: int; y: int; str: int }

let toPairs (line: string) =
    let a = line.Split(",")
    (int a[0], int a[1]), (int a[2], int a[3])

let mhDist (sx, sy) (bx, by) = (abs (sx - bx)) + (abs (sy - by))

let toSensor (s, b) =
    { x = fst s
      y = snd s
      str = mhDist s b }

let input =
    File
        .ReadAllText("sinput")
        .Replace("Sensor at x=", "")
        .Replace(" y=", "")
        .Replace(": closest beacon is at x=", ",")
        .Split("\n")
    |> Array.toList
    |> List.map toPairs

let frontier (s: Sensor) =
    let mutable f = []

    for i in s.x - s.str .. s.x + s.str do
        for j in s.y - s.str .. s.y + s.str do
            if mhDist (i, j) (s.x, s.y) = s.str + 1 then
                f <- f @ [ (i, j) ]

    f
    
let silver =
    let disc = 10

    let sensors =
        input |> List.map toSensor |> List.filter (fun s -> abs (s.y - disc) <= s.str)

    let knownXs =
        List.fold (fun acc (s: Sensor) -> acc @ [ s.x - s.str; s.x + s.str ]) [] sensors

    let allXs = [ List.min knownXs .. List.max knownXs ]

    let impBeacon =
        List.fold
            (fun acc x ->
                match List.exists (fun (s: Sensor) -> mhDist (s.x, s.y) (x, disc) <= s.str) sensors with
                | true -> acc + 1
                | false -> acc)
            0
            allXs

    let confirmedBeacons =
        input
        |> List.map (fun (_, b) -> b)
        |> List.filter (fun b -> snd b = disc)
        |> List.distinct
        |> List.length

    printfn "Result: %A" (impBeacon - confirmedBeacons)

let gold =
    let disc = 4000000
    let sensors = input |> List.map toSensor
    let candidates = sensors |> List.fold (fun acc s -> acc @ frontier s) []

    printfn "FOUND %A CANDIDATES" candidates.Length

    let beacon =
        candidates
        |> List.filter (fun c ->
            List.fold (fun acc (s: Sensor) -> acc && mhDist (s.x, s.y) (fst c, snd c) > s.str) true sensors)

    printfn "GOLD %A" beacon
