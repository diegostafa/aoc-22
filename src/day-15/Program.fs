open System.IO

type Sensor = { x: int; y: int; str: int }

let input =
    let toPairs (line: string) =
        let a = line.Split(",")
        (int a[0], int a[1]), (int a[2], int a[3])

    File
        .ReadAllText("input")
        .Replace("Sensor at x=", "")
        .Replace(" y=", "")
        .Replace(": closest beacon is at x=", ",")
        .Split("\n")
    |> Array.toList
    |> List.map toPairs

let mhDist (sx, sy) (bx, by) = (abs (sx - bx)) + (abs (sy - by))

let toSensor (s, b) =
    { x = fst s
      y = snd s
      str = mhDist s b }

let frontier (s: Sensor) =
    printfn "DOING %A" s
    let mutable f = []

    let sx = (s.x - s.str - 1, s.y)
    let dx = (s.x + s.str + 1, s.y)
    let tp = (s.x, s.y - s.str - 1)
    let bt = (s.x, s.y + s.str + 1)

    let mutable curr = sx

    while not (curr = tp) do
        f <- [ curr ] @ f
        curr <- (fst curr + 1, snd curr - 1)

    let mutable curr = tp

    while not (curr = dx) do
        f <- [ curr ] @ f
        curr <- (fst curr + 1, snd curr + 1)

    let mutable curr = dx

    while not (curr = bt) do
        f <- [ curr ] @ f
        curr <- (fst curr - 1, snd curr + 1)

    let mutable curr = bt

    while not (curr = sx) do
        f <- [ curr ] @ f
        curr <- (fst curr - 1, snd curr - 1)

    f

let silver =
    let disc = 2000000

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

    let candidates =
        sensors
        |> List.fold (fun acc s -> frontier s @ acc) []
        |> List.filter (fun (x, y) -> x >= 0 && x <= disc && y >= 0 && y <= disc)

    let beacon =
        candidates
        |> List.find (fun c ->
            List.fold
                (fun notInRange (s: Sensor) -> notInRange && mhDist (s.x, s.y) (fst c, snd c) > s.str)
                true
                sensors)

    printfn "Result: %A" ((int64 (fst beacon)) * int64 4000000 + int64 (snd beacon))
