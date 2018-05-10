open System

type DirectionsSummary = {
    N: int
    S: int
    SE: int
    SW: int
    NE: int
    NW: int
} with
    static member Initial = { N = 0; S = 0; SE = 0; SW = 0; NE = 0; NW = 0 }

let step1 = 
(* global idea is to reduce one dimension at each step *)
    let directionsList = 
        [| System.AppDomain.CurrentDomain.BaseDirectory; "res/input.txt" |]
        |> System.IO.Path.Combine
        |> System.IO.File.ReadAllLines
        |> Array.head
        |> fun line -> line.Split([| ',' |])
        |> Array.toList

    (* step1 = gather all directions occurrences *)
    let rec identifyDirections directions dirAccu = 
        match directions with
        | []            -> dirAccu
        | "n"::rest     -> identifyDirections rest { dirAccu with N = dirAccu.N + 1}
        | "s"::rest     -> identifyDirections rest { dirAccu with S = dirAccu.S + 1}
        | "se"::rest    -> identifyDirections rest { dirAccu with SE = dirAccu.SE + 1}
        | "sw"::rest    -> identifyDirections rest { dirAccu with SW = dirAccu.SW + 1}
        | "ne"::rest    -> identifyDirections rest { dirAccu with NE = dirAccu.NE + 1}
        | "nw"::rest    -> identifyDirections rest { dirAccu with NW = dirAccu.NW + 1}
        | _       -> failwith "Unknown direction"

    (* step2 = from 6 dimensions we go down to 4 : NE NW SE SW *)
    let reduceNorthsAndSouths directions = 
        { directions with N = 0; NE = directions.NE + directions.N; NW = directions.NW + directions.N;
                          S = 0; SE = directions.SE + directions.S; SW = directions.SW + directions.S }

    (* step3 = from 4 dimensions we keep only directions NE NW or SE SW to reduce to 2 dimensions only *)
    let reduceNESW_NWSE directions = 
        { directions with N = directions.N; S = directions.S; SE = 0; SW = 0
                          NE = abs (directions.NE - directions.SW); 
                          NW = abs (directions.NW - directions.SE); }

    (* step4 = NE and NW are not shortest so we must transform a dimension into N *)
    let reduceNorths directions = 
        if (directions.NE < directions.NW) then
            let tmp = { directions with NE = 0; NW = directions.NW - directions.NE; N = directions.N + directions.NE }
            tmp.N + tmp.NW
        else
            let tmp = { directions with NW = 0; NE = directions.NE - directions.NW; N = directions.N + directions.NW }
            tmp.N + tmp.NE

    identifyDirections directionsList DirectionsSummary.Initial
    |> reduceNorthsAndSouths
    |> reduceNESW_NWSE
    |> reduceNorths

[<EntryPoint>]
let main argv = 
    printfn "%A" step1
    System.Console.ReadLine() |> ignore
    0
