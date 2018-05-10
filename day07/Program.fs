open System
open System.Text.RegularExpressions

type RecTree = 
    | Node of Name:string * Weight:uint32 * Children:RecTree array option 

let step1V2() = 
    let registerParentNodeFromDescription (str:string) = 
        let parentName = 
            str.IndexOf('(') |> fun index -> index - 1 |> fun endex -> str.Substring(0, endex)
        let childrenNames = 
            str.IndexOf('>') |> fun index -> index + 2 |> fun startdex -> str.Substring(startdex).Split([| ", " |], StringSplitOptions.RemoveEmptyEntries)
        childrenNames |> Array.map (fun childName -> (childName, parentName))

    let childrenToParent = 
        [| AppDomain.CurrentDomain.BaseDirectory; "res/input.txt" |]
        |> System.IO.Path.Combine 
        |> System.IO.File.ReadAllLines 
        |> Array.filter (fun line -> line.Contains ">")
        |> Array.map registerParentNodeFromDescription
        |> Array.collect (fun childParentTpl -> childParentTpl)
        |> Map.ofArray

    let rec navigateToRoot (catalog:Map<string, string>) currentNode = 
        match catalog.ContainsKey currentNode with
        | false -> currentNode
        | true -> navigateToRoot catalog catalog.[currentNode]

    (* start with a random key = the first that has a length > 1*)
    childrenToParent 
    |> Map.tryFindKey (fun (k:string) _ -> k.Length > 1)
    |> Option.map (fun key -> navigateToRoot childrenToParent key)

let step2 () =
    (* sanitizes and extracts children of a parent node string *)
    let extractChildrenFrom str = 
        let patternSpaces = ", "
        let parChildren = Regex.Replace(str, patternSpaces, ",")
        let tmp = parChildren.Split([| "> " |], StringSplitOptions.RemoveEmptyEntries)
        tmp.[1] |> fun s -> s.Split ([| ',' |])

    (* gets an array of tuples parent/weight/children *)
    let registerNodeWeightChildren (nodeDescription:string) = 
        let patternNameWeight = "(\w+) \((\d+)\)"
        let nodeNameAndWeight = Regex.Match(nodeDescription, patternNameWeight)
        let children = 
            match nodeDescription.Contains(">") with
            | true -> extractChildrenFrom nodeDescription
            | false -> [| |]

        nodeNameAndWeight.Groups.[1].Value, (uint32(nodeNameAndWeight.Groups.[2].Value), children)

    let catalogNodesWeightChildren = 
        [| AppDomain.CurrentDomain.BaseDirectory; "res/input.txt" |]
        |> System.IO.Path.Combine 
        |> System.IO.File.ReadAllLines 
        |> Array.map registerNodeWeightChildren
        |> Map.ofArray

    (* do not try to do all the work in one func we will pass twice : build then check *)
    let rec buildTree (catalog:Map<string, (uint32 * string array)>) (nodeNameToProcess:string) = 
        let weight, children = catalog.[nodeNameToProcess]
        match children.Length with
        | 0 -> RecTree.Node(nodeNameToProcess, weight, None)
        | _ -> RecTree.Node(nodeNameToProcess, weight, Some(children |> Array.map (fun child -> buildTree catalog child)))

    let rec checkWeights (node:RecTree) = 
        match node with
        | Node(_, weight, None) -> weight
        | Node(name, weight, Some(children)) -> let childrenWeights = children |> Array.map (fun n -> checkWeights n)
                                                let areWeightsEqual = childrenWeights |> Array.distinct |> Array.length = 1
                                                match areWeightsEqual with
                                                | true -> ()
                                                | false -> printfn "one of these children has wrong weight. weight children %A" childrenWeights
                                                weight + (childrenWeights |> Array.sum)

    checkWeights (buildTree catalogNodesWeightChildren "dtacyn")

[<EntryPoint>]
let main argv = 
    step1V2() |> Option.iter (printfn "%s")

//    let a  = step2()
    printfn "Step has completed"
    (*nieyygi retourne 1122,1117,1117,1117 donc son premier enfant ptshrn est coupable -> on le balance de 5*)
    System.Console.ReadLine() |> ignore
    0