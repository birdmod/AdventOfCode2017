open System
open System.Text.RegularExpressions

let step1 () =
    let conditionRegex = Regex("(\w+) (==|>=|<=|!=|>|<) (-?\d+)")
    let instructionRegex = Regex("(\w+) (inc|dec) (-?\d+)")
    let instructionExtractor op = 
        match op with
        | "inc" -> (+) | "dec" | _ -> (-)
    let conditionExtractor op = 
        match op with
        | "==" -> (=) | ">=" -> (>=) | "<=" -> (<=) | "!=" -> (<>) | "<" -> (<) | ">" -> (>) | _ -> (=)

    let extractFrom (input:string) (regex:Regex) (logic:(string->'b)) = 
        let ``match`` = regex.Match(input)
        let register = ``match``.Groups.[1].Value
        let operation = logic ``match``.Groups.[2].Value
        let operand = (int)``match``.Groups.[3].Value
        (register, operation, operand)

    let processSingleInstructionLine (registers: Map<string, int>) (lineToProcess:string) = 
        (* helper *)
        let getRegisterValueOr0 requiredRegister = 
            match registers.ContainsKey requiredRegister with
            | true -> registers.[requiredRegister]
            | false -> 0

        let parts = lineToProcess.Split([| " if " |], StringSplitOptions.RemoveEmptyEntries)
        let condRegister, condOperation, condOperand = extractFrom parts.[1] conditionRegex conditionExtractor
        
        match condOperation (getRegisterValueOr0 condRegister) condOperand with
        | true -> let instrRegister, instrOperation, instrOperand = extractFrom parts.[0] instructionRegex instructionExtractor
                  let newRegisterValue = instrOperation (getRegisterValueOr0 instrRegister) instrOperand
                  registers |> Map.add instrRegister newRegisterValue
        | false -> registers
    
    System.IO.File.ReadLines (System.IO.Path.Combine [| System.AppDomain.CurrentDomain.BaseDirectory; "res/input.txt" |])
    |> Seq.fold (fun state line -> processSingleInstructionLine state line) Map.empty
    |> Map.toArray
    |> Array.maxBy (fun tpl -> snd tpl)
    |> snd

let step2 () =
    let conditionRegex = Regex("(\w+) (==|>=|<=|!=|>|<) (-?\d+)")
    let instructionRegex = Regex("(\w+) (inc|dec) (-?\d+)")
    let instructionExtractor op = 
        match op with
        | "inc" -> (+) | "dec" | _ -> (-)
    let conditionExtractor op = 
        match op with
        | "==" -> (=) | ">=" -> (>=) | "<=" -> (<=) | "!=" -> (<>) | "<" -> (<) | ">" -> (>) | _ -> (=)

    let extractFrom (input:string) (regex:Regex) (logic:(string->'b)) = 
        let ``match`` = regex.Match(input)
        let register = ``match``.Groups.[1].Value
        let operation = logic ``match``.Groups.[2].Value
        let operand = (int)``match``.Groups.[3].Value
        (register, operation, operand)

    let processSingleInstructionLine (registersAndMax: (Map<string, int> * int)) (lineToProcess:string)= 
        (* helper *)
        let getRegisterValueOr0 requiredRegister = 
            match (fst registersAndMax).ContainsKey requiredRegister with
            | true -> (fst registersAndMax).[requiredRegister]
            | false -> 0

        let parts = lineToProcess.Split([| " if " |], StringSplitOptions.RemoveEmptyEntries)
        let condRegister, condOperation, condOperand = extractFrom parts.[1] conditionRegex conditionExtractor
        
        match condOperation (getRegisterValueOr0 condRegister) condOperand with
        | true -> let instrRegister, instrOperation, instrOperand = extractFrom parts.[0] instructionRegex instructionExtractor
                  let newRegisterValue = instrOperation (getRegisterValueOr0 instrRegister) instrOperand
                  let newMax = if newRegisterValue >= (snd registersAndMax) then newRegisterValue else (snd registersAndMax)
                  ((fst registersAndMax) |> Map.add instrRegister newRegisterValue, newMax)
        | false -> registersAndMax
    
    System.IO.File.ReadLines (System.IO.Path.Combine [| System.AppDomain.CurrentDomain.BaseDirectory; "res/input.txt" |])
    |> Seq.fold (fun state line -> processSingleInstructionLine state line) (Map.empty, Int32.MinValue)
    |> snd


[<EntryPoint>]
let main argv = 
    let res = step2()
    printfn "%i" res
    Console.ReadLine() |> ignore
    0