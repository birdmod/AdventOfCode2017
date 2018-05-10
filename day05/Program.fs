
let step1 = 
    let input = 
        System.IO.File.ReadLines (System.IO.Path.Combine [| System.AppDomain.CurrentDomain.BaseDirectory; "res/input.txt" |])
        |> Seq.map (fun s -> int (s)) |> Seq.toArray

    let rec loopInstr (instructions: int array) (currentIndex:int) (iterations:int) = 
        match currentIndex with
        | n when (n >= instructions.Length || n < 0) -> iterations
        | _ ->
            let newIndex = currentIndex + instructions.[currentIndex]
            instructions.[currentIndex] <- instructions.[currentIndex] + 1
            loopInstr instructions newIndex (iterations + 1)

    loopInstr input 0 0


let step2 = 
    let input = 
        System.IO.File.ReadLines (System.IO.Path.Combine [| System.AppDomain.CurrentDomain.BaseDirectory; "res/input.txt" |])
        |> Seq.map (fun s -> int (s)) |> Seq.toArray

    let rec loopInstr (instructions: int array) (currentIndex:int) (iterations:int) = 
        match currentIndex with
        | n when (n >= instructions.Length || n < 0) -> iterations
        | _ ->
            let newIndex = currentIndex + instructions.[currentIndex]            
            instructions.[currentIndex] <- 
                match instructions.[currentIndex] with
                | n when n >= 3 -> instructions.[currentIndex] - 1
                | _ -> instructions.[currentIndex] + 1

            loopInstr instructions newIndex (iterations + 1)

    loopInstr input 0 0


[<EntryPoint>]
let main argv = 
    printfn "%i stems" step2
    System.Console.ReadLine() |> ignore
    0