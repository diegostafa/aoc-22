open System.IO

module Part1 =
    let isHidden (x, y) (grid: char[,]) =
        let hidden wasHidden neighbor = wasHidden || grid[x, y] <= neighbor

        Array.fold hidden false grid[x, (y + 1) ..]
        && Array.fold hidden false grid[x, .. (y - 1)]
        && Array.fold hidden false grid[.. (x - 1), y]
        && Array.fold hidden false grid[(x + 1) .., y]

    let solve =
        let arr2d = File.ReadAllText("input").Split("\n") |> Array.map Seq.toArray
        let grid = Array2D.init arr2d.Length arr2d[0].Length (fun x y -> arr2d[x][y])
        let mutable vis = Array2D.length1 grid * 2 + (Array2D.length2 grid - 2) * 2

        for x in 1 .. Array2D.length1 grid - 2 do
            for y in 1 .. Array2D.length2 grid - 2 do
                if not (isHidden (x, y) grid) then
                    vis <- vis + 1

        printfn "Result: %i" vis

module Part2 =
    let viewingDistance (x, y) (grid: char[,]) =
        let countVisibleTrees (tot, blocked) neighbor =
            match (tot, blocked) with
            | (_, true) -> (tot, blocked)
            | (t, false) -> (t + 1, (neighbor >= grid[x, y]))

        (fst (Array.fold countVisibleTrees (0, false) grid[(x + 1) .., y]))
        * (fst (Array.fold countVisibleTrees (0, false) (Array.rev grid[.. (x - 1), y])))
        * (fst (Array.fold countVisibleTrees (0, false) grid[x, (y + 1) ..]))
        * (fst (Array.fold countVisibleTrees (0, false) (Array.rev grid[x, .. (y - 1)])))

    let solve =
        let arr2d = File.ReadAllText("input").Split("\n") |> Array.map Seq.toArray
        let grid = Array2D.init arr2d.Length arr2d[0].Length (fun x y -> arr2d[x][y])
        let mutable (viewDists: int list) = []

        for x in 0 .. Array2D.length1 grid - 1 do
            for y in 0 .. Array2D.length2 grid - 1 do
                viewDists <- viewDists @ [ (viewingDistance (x, y) grid) ]

        printfn "Result: %i" (List.max viewDists)
