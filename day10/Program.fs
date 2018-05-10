
let step1 = 
    let elementsToReverseList = 
        [| System.AppDomain.CurrentDomain.BaseDirectory; "res/input.txt" |]
        |> System.IO.Path.Combine 
        |> System.IO.File.ReadAllLines
        |> Array.head
        |> fun line -> line.Split([| ',' |])
        |> Array.map (fun s -> int(s))
        |> Array.toList

    let swapAtIndexes (xs:int array) (i1:int) (i2:int) = 
        let copy = Array.copy xs
        let tmp = copy.[i1]
        copy.[i1] <- copy.[i2]
        copy.[i2] <- tmp
        copy

    let swapAllIndexesInArray (xs:int array) (revIndexes:int array) = 
        (*cut them in half so we do not cancel the swaps by performing them twice *)
        let originalOrderIndexes = Array.rev revIndexes
        let halfReversed = revIndexes.[0..(revIndexes.Length/2) - 1]
        let halfOriginal = originalOrderIndexes.[0..(originalOrderIndexes.Length/2) - 1]
        Array.fold2 swapAtIndexes xs halfOriginal halfReversed

    (* returns from index the remIters indexes reversed *)
    let getConcernedIndexesThenReverse = 
        let rec recExtractAndReverse (accu:int list) (xs:int array) (index:int) (remIters:int) = 
            match remIters with
            | 0 -> accu
            | n -> let adjustedIndex = index % xs.Length
                   recExtractAndReverse (adjustedIndex::accu) xs (index + 1) (n - 1)
        recExtractAndReverse []

    let rec twistInput (inputList: int array) (index:int) (accumulatedStep:int) (nbToReverseList:int list) = 
        match nbToReverseList with
        | [] -> inputList
        | nbToReverse::l -> let reversedIndexes = (getConcernedIndexesThenReverse inputList index nbToReverse) |> List.toArray
                            let inputWithReversedElements = swapAllIndexesInArray inputList reversedIndexes
                            let newIndex = (index + nbToReverse + accumulatedStep) % inputList.Length
                            twistInput inputWithReversedElements newIndex (accumulatedStep + 1) l


    twistInput [| 0..255 |] 0 0 elementsToReverseList

let step2 = 
    let toAdd = "17,31,73,47,23".Split([|','|]) |> Array.map int

    let elementsToReverseList = 
        System.IO.Path.Combine [| System.AppDomain.CurrentDomain.BaseDirectory; "res/input.txt" |]
        |> System.IO.File.ReadAllLines
        |> Array.head
        |> fun line -> line.ToCharArray()
        |> Array.map int
        |> fun xs -> Array.concat [| xs; toAdd |]
        |> Array.toList

    let swapAtIndexes (xs:int array) (i1:int) (i2:int) = 
        let copy = Array.copy xs
        let tmp = copy.[i1]
        copy.[i1] <- copy.[i2]
        copy.[i2] <- tmp
        copy

    let swapAllIndexesInArray (xs:int array) (revIndexes:int array) = 
        (*cut them in half so we do not cancel the swaps by performing them twice *)
        let originalOrderIndexes = Array.rev revIndexes
        let halfReversed = revIndexes.[0..(revIndexes.Length/2) - 1]
        let halfOriginal = originalOrderIndexes.[0..(originalOrderIndexes.Length/2) - 1]
        Array.fold2 swapAtIndexes xs halfOriginal halfReversed

    (* returns from index the remIters indexes reversed *)
    let getConcernedIndexesThenReverse = 
        let rec recExtractAndReverse (accu:int list) (xs:int array) (index:int) (remIters:int) = 
            match remIters with
            | 0 -> accu
            | n -> let adjustedIndex = index % xs.Length
                   recExtractAndReverse (adjustedIndex::accu) xs (index + 1) (n - 1)
        recExtractAndReverse []

    let rec twistSingleRound (inputList: int array) (index:int) (accumulatedStep:int) (nbToReverseList:int list) = 
        match nbToReverseList with
        | [] -> inputList, index, accumulatedStep
        | nbToReverse::l -> let reversedIndexes = (getConcernedIndexesThenReverse inputList index nbToReverse) |> List.toArray
                            let inputWithReversedElements = swapAllIndexesInArray inputList reversedIndexes
                            let newIndex = (index + nbToReverse + accumulatedStep) % inputList.Length
                            twistSingleRound inputWithReversedElements newIndex (accumulatedStep + 1) l

    let rec twistNRounds (nbRounds:int) (index:int) (accumulatedStep:int) (inputList: int array) (nbToReverseList:int list) = 
        match nbRounds with
        | 0 -> inputList
        | n -> let resultingList, newIndex, newAccumulatedStep = twistSingleRound inputList index accumulatedStep nbToReverseList
               twistNRounds (n-1) newIndex newAccumulatedStep resultingList nbToReverseList

    let sparseHash = 
        twistNRounds 64 0 0 [| 0..255 |] elementsToReverseList

    let fold3rXOR (state:int option) (curElt:int) = 
        match state with
        | None -> Some(curElt)
        | Some(v) -> Some(v ^^^ curElt) // XOR = Exclusive OR = ^^^

    let denseHash = 
        sparseHash
        |> Array.chunkBySize 16
        |> Array.map (fun blockOf16 -> Array.fold fold3rXOR None blockOf16)
        |> Array.map (fun x -> Option.get x)
        |> Array.map (fun x -> x.ToString("X2"))


    denseHash

[<EntryPoint>]
let main argv = 
    printfn "%A" step2
    System.Console.ReadLine() |> ignore
    0