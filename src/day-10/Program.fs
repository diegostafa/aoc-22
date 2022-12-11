open System.IO

type Cmd =
    | Noop
    | Addx of int

let parseCmd (cmd: string) =
    match cmd.Split(" ") |> Seq.toList with
    | _ :: [] -> [ Noop ]
    | _ :: reg :: [] -> [ Noop; (Addx(int reg)) ]
    | _ -> []

let evalProg (regCycles: int list, regVal) (cmd: Cmd) =
    match cmd with
    | Noop -> (regCycles @ [ regVal ]), regVal
    | Addx reg -> (regCycles @ [ regVal ]), regVal + reg

let silver =
    let regHist, _ =
        File.ReadAllText("input").Split("\n")
        |> Array.toList
        |> List.collect parseCmd
        |> List.fold evalProg ([ 1 ], 1)

    printfn "Result: %A" (List.fold (fun acc i -> acc + regHist[i] * i) 0 [ 20..40 .. regHist.Length ])

let gold =
    let regHist, _ =
        File.ReadAllText("input").Split("\n")
        |> Array.toList
        |> List.collect parseCmd
        |> List.fold evalProg ([ 1 ], 1)

    let regs = List.chunkBySize 40 (List.tail regHist)

    List.iter
        (fun row ->
            List.iter
                (fun col ->
                    if col >= regs[row][col] - 1 && col <= regs[row][col] + 1 then
                        printf "#"
                    else
                        printf " "

                    if col = (regs[row].Length - 1) then
                        printfn "")
                [ 0 .. (regs[row].Length - 1) ])
        [ 0 .. (regs.Length - 1) ]
