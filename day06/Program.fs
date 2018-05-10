open System


type MemoryBanks = int array

let step1 () = 
    let runningFolder = AppDomain.CurrentDomain.BaseDirectory
    let fullPath = System.IO.Path.Combine [| runningFolder ; "input.txt" |]
    let input = System.IO.File.ReadAllLines fullPath
    
    let inputToMemoryBanks:MemoryBanks = 
        input 
        |> Array.head
        |> fun line -> line.Split [| '\t' |]
        |> Array.map (fun strBlock -> int(strBlock))
    
    (* Cannot use maxBy as we need the lowest index of the max value in case of tie between blocks *)
    let getLowestIndexOfMaxValue (xs:MemoryBanks) = 
        let toIndexValueTuple = xs |> Array.mapi (fun idx value -> (idx, value))
        let fold3r (tpl:int*int) (state:int*int) = if (snd tpl) >= (snd state) then tpl else state
        Array.foldBack fold3r toIndexValueTuple (0, Int32.MinValue)

    (* distributes the block pond on all other memory bank blocks *)
    let dryBlockPondOptimized (memBank:MemoryBanks) (pondBlock:int) (startPosition:int) =
        
        let rec dryRemainderBlocks (memBank2:MemoryBanks) (pondBlock2:int) (startPosition2:int) = 
            match pondBlock2 with 
            | 0 -> memBank2 (* when the pond is dried = reallocation on all banks *)
            | _ when startPosition2 >= Array.length memBank2 -> dryRemainderBlocks memBank2 pondBlock2 0 (* handles cycling *)
            | _ -> memBank2.[startPosition2] <- memBank2.[startPosition2] + 1
                   dryRemainderBlocks memBank2 (pondBlock2 - 1) (startPosition2 + 1)

        let nbBlocksSplit = pondBlock / (Array.length memBank)
        let newMemoryBanks = memBank |> Array.map (fun existingBlocks -> existingBlocks + nbBlocksSplit)

        let nbBlocksSplitRemainder = pondBlock % (Array.length memBank)
        dryRemainderBlocks newMemoryBanks nbBlocksSplitRemainder startPosition

    let rec isCurrentAlreadyMet (current: MemoryBanks) (pastMemoryBanks: MemoryBanks list) = 
        match pastMemoryBanks with
        | [] -> false
        | past::_ when (System.Linq.Enumerable.SequenceEqual (current, past) = true) -> true
        | _::others -> isCurrentAlreadyMet current others

    let rec isCurrentAlreadyMetStep2 (current: MemoryBanks) (pastMemoryBanks: MemoryBanks list) (diff:int) = 
        match pastMemoryBanks with
        | [] -> None
        | past::_ when (System.Linq.Enumerable.SequenceEqual (current, past) = true) -> Some(diff)
        | _::others -> isCurrentAlreadyMetStep2 current others (diff + 1)

    let rec realloc (xs: MemoryBanks) (pastMemBanks: MemoryBanks list) (iters:int) =
        let maxValIndex, maxValValue = getLowestIndexOfMaxValue xs
        // pre dry
        let copy = Array.copy xs
        copy.[maxValIndex] <- 0
        
        // drying operation
        let reallocated = dryBlockPondOptimized copy maxValValue (maxValIndex + 1)

        (* step1 *)
        (*
        let alreadyMet = isCurrentAlreadyMet reallocated pastMemBanks
        match alreadyMet with
        | false -> realloc reallocated (xs::pastMemBanks) (iters + 1)
        | true -> iters
        *)

        (* step 2*)
        let repeatingCycles = isCurrentAlreadyMetStep2 reallocated pastMemBanks 1

        match repeatingCycles with
        | None -> realloc reallocated (xs::pastMemBanks) (iters + 1)
        | Some(repeatingCyclesNb) -> repeatingCyclesNb + 1

    realloc inputToMemoryBanks [] 1

[<EntryPoint>]
let main argv = 
    let res = step1 ()
    printfn "cycles : %i" res
    Console.ReadLine() |> ignore
    0
