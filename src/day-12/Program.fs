open System.IO

let arr = File.ReadAllText("input").Split("\n") |> Array.map Seq.toArray
let grid = Array2D.init arr.Length arr[0].Length (fun x y -> arr[x][y])
let maxCost = 99999999

let mutable s = (0, 0)
let mutable e = (0, 0)
let mutable ss = []
let mutable (visited: (int * int) list) = []

let possibleMoves (x, y) (grid: char[,]) =
    [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]
    |> List.filter (fun (i, j) ->
        (   not (i = -1)
         && not (j = -1)
         && not (i = Array2D.length1 grid)
         && not (j = Array2D.length2 grid))
         && not (visited |> List.contains (i, j))
         && not (int grid[i, j] > (int grid[x, y] + 1)))

let rec minCost wave e grid =
    if List.contains e wave then
        0
    elif wave.IsEmpty then
        maxCost
    else
        for cell in wave do
            visited <- [ cell ] @ visited

        let newWave = List.fold (fun f c -> List.distinct f @ possibleMoves c grid) [] wave

        if wave = newWave then
            maxCost
        else
            1 + minCost newWave e grid

let silver =
    visited <- []

    for i in 0 .. Array2D.length1 grid - 1 do
        for j in 0 .. Array2D.length2 grid - 1 do
            match (char grid[i, j]) with
            | 'S' -> s <- (i, j)
            | 'E' -> e <- (i, j)
            | _ -> ()

    Array2D.set grid (fst s) (snd s) 'a'
    Array2D.set grid (fst e) (snd e) 'z'

    printfn "Result: %A" (minCost [ s ] e grid)

let gold =

    for i in 0 .. Array2D.length1 grid - 1 do
        for j in 0 .. Array2D.length2 grid - 1 do
            match (char grid[i, j]) with
            | 'S' ->
                s <- (i, j)
                ss <- ss @ [ (i, j) ]
            | 'a' -> ss <- ss @ [ (i, j) ]
            | 'E' -> e <- (i, j)
            | _ -> ()

    Array2D.set grid (fst s) (snd s) 'a'
    Array2D.set grid (fst e) (snd e) 'z'

    printfn
        "%A"
        (ss
         |> List.map (fun s ->
             visited <- []
             minCost [ s ] e grid)
         |> List.min)