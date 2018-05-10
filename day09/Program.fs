open System

type AdventStack = 
    | EmptyStack
    | Stack of char * AdventStack

    member self.Push v = 
        Stack(v, self)

    member self.Unstack = 
        match self with
        | EmptyStack -> EmptyStack
        | Stack(_, tail) -> tail

//    {}, score of 1.
//    {{{}}}, score of 1 + 2 + 3 = 6.
//    {{},{}}, score of 1 + 2 + 2 = 5.
//    {{{},{},{{}}}}, score of 1 + 2 + 3 + 3 + 3 + 4 = 16.
//    {<a>,<a>,<a>,<a>}, score of 1.
//    {{<ab>},{<ab>},{<ab>},{<ab>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
//    {{<!!>},{<!!>},{<!!>},{<!!>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
//    {{<a!>},{<a!>},{<a!>},{<ab>}}, score of 1 + 2 = 3.

let step1 =
    let rec processInput (stack:AdventStack) (score:int) (isGarbage:bool) (input:char list) =
        match input with
        | [] -> 0
        | c::remain -> match isGarbage, c, remain with
                       | true, '>', tail -> processInput stack score false tail
                       | true, '!', _::tail -> processInput stack score true tail (* see how we skipped one char *)
                       | true, _, tail -> processInput stack score true tail
                       | _, '<', tail -> processInput stack score true tail
                       | _, '{', tail -> processInput (stack.Push '{') (score + 1) isGarbage tail
                       | _, '}', tail -> score + (processInput (stack.Unstack) (score - 1) isGarbage tail)
                       | _, ',', tail -> processInput (stack.Push '{') score isGarbage tail
                       | _, _, _ -> score

    System.IO.Path.Combine [| AppDomain.CurrentDomain.BaseDirectory; "res/input.txt" |]
    |> System.IO.File.ReadAllLines
    |> Array.head (* there is only one line *)
    |> fun str -> str.ToCharArray()
    |> Array.toList
    |> processInput EmptyStack 0 false

let step2 =
    let rec processInput (stack:AdventStack) (score:int) (isGarbage:bool) (nbGarbageChars:int) (input:char list) =
        match input with
        | [] -> 0, nbGarbageChars
        | c::remain -> match isGarbage, c, remain with
                       | true, '>', tail -> processInput stack score false nbGarbageChars tail
                       | true, '!', _::tail -> processInput stack score true nbGarbageChars tail (* see how we skipped one char *)
                       | true, _, tail -> processInput stack score true (nbGarbageChars + 1) tail
                       | _, '<', tail -> processInput stack score true  nbGarbageChars tail
                       | _, '{', tail -> processInput (stack.Push '{') (score + 1) isGarbage nbGarbageChars tail
                       | _, '}', tail -> let res = (processInput stack.Unstack (score - 1) isGarbage nbGarbageChars tail)
                                         (score + (fst res), snd res)
                       | _, ',', tail -> processInput (stack.Push '{') score isGarbage nbGarbageChars tail
                       | _, _, _ -> score, nbGarbageChars

    System.IO.Path.Combine [| AppDomain.CurrentDomain.BaseDirectory; "res/input.txt" |]
    |> System.IO.File.ReadAllLines
    |> Array.head (* there is only one line *)
    |> fun str -> str.ToCharArray()
    |> Array.toList
    |> processInput EmptyStack 0 false 0

[<EntryPoint>]
let main argv = 
    let a, b = step2
    printfn "%i %i" a b 
    Console.ReadLine() |> ignore
    0
