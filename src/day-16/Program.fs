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
        .ReadAllText("input")
        .Replace("Valve ", "")
        .Replace(" has flow rate=", ",")
        .Replace("; tunnels lead to valves ", ",")
        .Replace("; tunnel leads to valve ", ",")
        .Replace(" ", "")
        .Split("\n")
    |> Array.toList
    |> List.map toValve

let getNeighbors v =
    valves |> List.filter (fun vt -> (List.contains vt.id v.conns))

let mutable (memoSilver: Map<string * int * Map<string, unit>, int>) = Map.empty

let mutable (memoGold: Map<string * string * int * int * Map<string, unit> * bool, int>) =
    Map.empty

let rec maxPressureSilver v time openValves =
    match () with
    | _ when memoSilver.ContainsKey(v.id, time, openValves) -> memoSilver.[v.id, time, openValves]
    | _ when time <= 0 -> 0
    | _ when openValves.Count = valves.Length -> 0
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

        memoSilver <- Map.add (v.id, time, openValves) maxPressure memoSilver
        maxPressure

let rec maxPressureGold (v1, v2) (t1, t2) openValves yourTurn =
    match () with
    | _ when t1 <= 0 || t2 <= 0 -> 0
    | _ when memoGold.ContainsKey(v1.id, v2.id, t1, t2, openValves, yourTurn) ->
        memoGold[v1.id, v2.id, t1, t2, openValves, yourTurn]
    | _ when openValves.Count = valves.Length -> 0
    | _ when yourTurn ->
        let nextValves = getNeighbors v1 |> List.filter (fun v -> not (v.id = v2.id))
        let mutable maxPressure = 0

        for n in nextValves do
            let leaveValve = maxPressureGold (n, v2) (t1 - 1, t2) openValves (not yourTurn)

            let openValve =
                if Map.containsKey v1.id openValves || v1.pressure = 0 then
                    0
                else
                    v1.pressure * (t1 - 1)
                    + maxPressureGold (n, v2) (t1 - 2, t2) (Map.add v1.id () openValves) (not yourTurn)

            maxPressure <- maxPressure |> max leaveValve |> max openValve

        memoGold <- Map.add (v1.id, v2.id, t1, t2, openValves, yourTurn) maxPressure memoGold
        maxPressure
    | _ when not yourTurn ->
        let nextValves = getNeighbors v2 |> List.filter (fun v -> not (v.id = v1.id))
        let mutable maxPressure = 0

        for n in nextValves do
            let leaveValve = maxPressureGold (v1, n) (t1, t2 - 1) openValves (not yourTurn)

            let openValve =
                if Map.containsKey v2.id openValves || v2.pressure = 0 then
                    0
                else
                    v2.pressure * (t2 - 1)
                    + maxPressureGold (v1, n) (t1, t2 - 2) (Map.add v2.id () openValves) (not yourTurn)

            maxPressure <- maxPressure |> max leaveValve |> max openValve

        memoGold <- Map.add (v1.id, v2.id, t1, t2, openValves, yourTurn) maxPressure memoGold
        maxPressure
    | _ -> failwith "UNMATCHED CASE"

let gold =
    let time = 26
    let start = List.find (fun v -> v.id = "AA") valves
    printfn "Result: %A" (maxPressureGold (start, start) (time, time) Map.empty true)
