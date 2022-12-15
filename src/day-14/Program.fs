open System.IO

let source = '+'
let sand = '.'
let rock = '#'
let air = ' '
let water = '~'

let toPair (str: string) =
    let a = str.Split(",")
    (int a[1], int a[0])

let rockss =
    File.ReadAllText("input").Split("\n")
    |> Array.toList
    |> List.map (fun (l: string) -> l.Split(" -> ") |> Array.toList)
    |> List.map (List.map toPair)

let rec grainsToWater s (grid: char[,]) =
    let rec drop (x, y) =
        if grid[x + 1, y] = water then (x, y)
        else if grid[x + 1, y] = air then drop (x + 1, y)
        elif grid[x + 1, y - 1] = air then drop (x + 1, y - 1)
        elif grid[x + 1, y + 1] = air then drop (x + 1, y + 1)
        else (x, y)

    let (x, y) = drop s

    if grid[x + 1, y] = water then
        0
    else
        Array2D.set grid x y sand
        1 + (grainsToWater s grid)

let rec grainsToSource s (grid: char[,]) =
    let rec drop (x, y) =
        if grid[x + 1, y] = air then drop (x + 1, y)
        elif grid[x + 1, y - 1] = air then drop (x + 1, y - 1)
        elif grid[x + 1, y + 1] = air then drop (x + 1, y + 1)
        else (x, y)

    let (x, y) = drop s

    if s = (x, y) then
        1
    else
        Array2D.set grid x y sand
        1 + (grainsToSource s grid)

let setRocks grid rockss =
    List.iter
        (fun rocks ->
            List.zip rocks ((List.tail rocks) @ [ rocks.Head ])
            |> List.rev
            |> List.tail
            |> List.iter (fun ((sx, sy), (ex, ey)) ->
                match (sx - ex), (sy - ey) with
                | (a, 0) when not (a = 0) ->
                    let m = min sx ex
                    List.iter (fun i -> Array2D.set grid i sy rock) [ m .. m + abs a ]
                | (0, a) when not (a = 0) ->
                    let m = min sy ey
                    List.iter (fun i -> Array2D.set grid sx i rock) [ m .. m + abs a ]
                | _ -> failwith ""))
        rockss

let setSource grid (x, y) = Array2D.set grid x y source

let bounds rockss =
    let x =
        List.fold (fun acc rocks -> acc @ (List.fold (fun acc rock -> acc @ [ fst rock ]) [] rocks)) [] rockss

    let y =
        List.fold (fun acc rocks -> acc @ (List.fold (fun acc rock -> acc @ [ snd rock ]) [] rocks)) [] rockss

    (List.max x + 1, List.min y - 1, List.max y + 1)

let silver =
    let (rowMax, colMin, colMax) = bounds rockss
    let colMax = colMax - colMin

    let rockss =
        List.map (fun rocks -> List.map (fun (x, y) -> (x, y - colMin)) rocks) rockss

    let s = (0, 500 - colMin)
    let grid = Array2D.init (rowMax + 1) (colMax + 1) (fun _ _ -> air)
    setRocks grid rockss
    setSource grid s

    for i in 0..colMax do
        Array2D.set grid (Array2D.length1 grid - 1) i water

    printfn "Result: %A" (grainsToWater s grid)

let gold =
    let (rowMax, _, _) = bounds rockss
    let colMax = 1000
    let s = (0, 500)
    let grid = Array2D.init (rowMax + 2) (colMax + 1) (fun _ _ -> air)
    setRocks grid rockss
    setSource grid s

    for i in 0..rowMax do
        Array2D.set grid i 0 rock
        Array2D.set grid i (Array2D.length1 grid - 1) rock

    for i in 0..colMax do
        Array2D.set grid (Array2D.length1 grid - 1) i rock

    printfn "Result: %A" (grainsToSource s grid)
