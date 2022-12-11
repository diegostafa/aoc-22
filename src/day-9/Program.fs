open System.IO

let toPair (line: string) =
    let tokens = line.Split(" ")

    match tokens[0] with
    | "U" -> ((0, 1), int tokens[1])
    | "D" -> ((0, -1), int tokens[1])
    | "R" -> ((1, 0), int tokens[1])
    | "L" -> ((-1, 0), int tokens[1])
    | _ -> ((0, 0), int tokens[1])

let updateStepCount (grid: Map<string, int>) coord =
    let key = coord |> string

    match grid.TryFind key with
    | Some stepCount -> Map.add key (stepCount + 1) grid
    | None -> Map.add key 1 grid

let updateDir (x1, y1) (x2, y2) =
    let hDist = x1 - x2
    let vDist = y1 - y2

    if hDist > 1 then
        if vDist >= 1 then (1, 1)
        elif vDist <= -1 then (1, -1)
        else (1, 0)
    elif hDist < -1 then
        if vDist >= 1 then (-1, 1)
        elif vDist <= -1 then (-1, -1)
        else (-1, 0)
    elif vDist > 1 then
        if hDist >= 1 then (1, 1)
        elif hDist <= -1 then (-1, 1)
        else (0, 1)
    elif vDist < -1 then
        if hDist >= 1 then (1, -1)
        elif hDist <= -1 then (-1, -1)
        else (0, -1)
    else
        (0, 0)

let move (knots, grid) ((dirx, diry), steps) =
    List.fold
        (fun (ks, grid) _ ->
            let newKnots, _ =
                List.fold
                    (fun (processed, index) (kx, ky) ->
                        match index with
                        | 0 -> [ (kx + dirx, ky + diry) ], 1
                        | n ->
                            let (newDx, newDy) = updateDir processed[n - 1] (kx, ky)
                            processed @ [ (kx + newDx, ky + newDy) ], (n + 1))
                    ([], 0)
                    ks

            if (List.last newKnots) = (List.last knots) then
                (newKnots, grid)
            else
                (newKnots, updateStepCount grid (List.last newKnots))

            )
        (knots, grid)
        [ 0 .. (steps - 1) ]

let silver =
    match
        File.ReadAllText("input").Split("\n")
        |> Seq.toList
        |> List.map toPair
        |> List.fold move ((List.replicate 2 (0, 0)), (updateStepCount Map.empty (0, 0)))
    with
    | (_, g) -> printfn "Result: %A" (Map.count g)

let gold =
    match
        File.ReadAllText("input").Split("\n")
        |> Seq.toList
        |> List.map toPair
        |> List.fold move ((List.replicate 10 (0, 0)), (updateStepCount Map.empty (0, 0)))
    with
    | (_, g) -> printfn "Result: %A" (Map.count g)
