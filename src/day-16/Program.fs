open System.IO

type Valve =
    { id: string
      pressure: int
      conns: string list }

let valves =
    let toValve (line: string) =
        match line.Split(",") |> Array.toList with
        | id :: pressure :: conns ->
            { id = id
              pressure = int pressure
              conns = conns }
        | _ -> failwith "INVALID INPUT"

    File
        .ReadAllText("sinput")
        .Replace("Valve ", "")
        .Replace(" has flow rate=", ",")
        .Replace("; tunnels lead to valves ", ",")
        .Replace("; tunnel leads to valve ", ",")
        .Replace(" ", "")
        .Split("\n")
    |> Array.toList
    |> List.map toValve

let getNeighbors v =
    List.filter (fun vt -> (List.contains vt.id v.conns)) valves

let mutable (memoSilver: Map<Valve * int * Map<string, unit>, int>) = Map.empty

let mutable (memoGold: Map<(Valve * Valve) * int * Map<string, unit>, int>) =
    Map.empty

let rec maxPressureSilver (v: Valve) (time: int) openValves =
    match () with
    | _ when memoSilver.ContainsKey(v, time, openValves) -> memoSilver[v, time, openValves]
    | _ when time <= 0 -> 0
    | _ when openValves.Keys.Count = valves.Length -> 0
    | _ ->
        let nextValves = getNeighbors v
        let mutable maxPressure = 0

        for n in nextValves do
            let leaveValve = maxPressureSilver n (time - 1) openValves

            let openValve =
                if v.pressure = 0 || Map.containsKey v.id openValves then
                    0
                else
                    v.pressure * (time - 1)
                    + maxPressureSilver n (time - 2) (Map.add v.id () openValves)

            maxPressure <- maxPressure |> max leaveValve |> max openValve

        memoSilver <- Map.add (v, time, openValves) maxPressure memoSilver
        maxPressure

let rec maxPressureGold (v1: Valve, v2: Valve) time (openValves: Map<string, unit>) =
    match () with
    | _ when memoGold.ContainsKey((v1, v2), time, openValves) -> memoGold[(v1, v2), time, openValves]
    | _ when time <= 0 -> 0
    | _ when openValves.Keys.Count = valves.Length -> 0
    | _ ->
        let nextValves1 = getNeighbors v1
        let nextValves2 = getNeighbors v2
        let mutable maxPressure = 0

        for n1 in nextValves1 do
            for n2 in nextValves2 do
                let leaveBoth = maxPressureGold (n1, n2) (time - 1) openValves

                let openV1 =
                    if v1.pressure = 0 || Map.containsKey v1.id openValves then
                        0
                    else
                        v1.pressure * (time - 1)
                        + maxPressureGold (n1, n2) (time - 2) (Map.add v1.id () openValves)

                let openV2 =
                    if v2.pressure = 0 || Map.containsKey v2.id openValves then
                        0
                    else
                        v2.pressure * (time - 1)
                        + maxPressureGold (n1, n2) (time - 2) (Map.add v2.id () openValves)

                let openBoth =
                    if
                        (v1.pressure = 0 && v2.pressure = 0)
                        || Map.containsKey v2.id openValves
                        || Map.containsKey v2.id openValves
                    then
                        0
                    elif v1.id = v2.id then
                        v1.pressure * (time - 1)
                        + maxPressureGold (n1, n2) (time - 2) (Map.add v1.id () (Map.add v2.id () openValves))
                    else
                        v1.pressure * (time - 1)
                        + v2.pressure * (time - 1)
                        + maxPressureGold (n1, n2) (time - 2) (Map.add v1.id () (Map.add v2.id () openValves))

                maxPressure <- maxPressure |> max leaveBoth |> max openV1 |> max openV2 |> max openBoth

        memoGold <- Map.add ((v1, v2), time, openValves) maxPressure memoGold
        maxPressure

let silver =
    let time = 30
    let start = List.find (fun v -> v.id = "AA") valves
    printfn "Result: %A" (maxPressureGold (start, start) time Map.empty)