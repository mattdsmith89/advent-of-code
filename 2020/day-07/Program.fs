// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO
open System.Text.RegularExpressions

module Helpers =
    let readLines (filePath: string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

module Solver =
    let (|Regex|_|) pattern input = 
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let getBagMap (input: string seq) =
        input 
        |> Seq.mapi (fun i x -> ((x.Split " bags contain").[0], i)) 
        |> Map

    let containerMapper (bagMap: Map<string,int>) (input: string) =
        let parts = input.Split " bags contain "
        let containerId = bagMap.[parts.[0]]
        let contents = parts.[1].Split(", ")
        let parsedContents =
            contents
            |> Array.choose (fun x ->
                if x.StartsWith("no") then None
                else match x with
                        | Regex @"([\d]*) ([\w]* [\w]*)" [ count; bag; ] -> 
                            Some (int count, bagMap.[bag])
                        | _ -> None)
        (containerId, parsedContents)

    let getBagContainers (bagMap: Map<string,int>) (input: string seq) =
        let containerMapperWithMap = containerMapper bagMap
        input
        |> Seq.map containerMapperWithMap
        |> Map

    let rec holdsBag (allBags: Map<int, (int * int) []>) (bagToCheck: int) (currentBag: (int * int) []) =
        let bags = currentBag |> Array.map snd
        let checkNextBag elem =
            let nextBag = allBags.[elem]
            holdsBag allBags bagToCheck nextBag
        if Array.contains bagToCheck bags then true
        else Array.exists checkNextBag bags

    let rec countBags (allBags: Map<int, (int * int) []>) (bagId: int) =
        let bag = allBags.[bagId]
        Array.sumBy (fun (subBagCount, subBagId) -> subBagCount + subBagCount * (countBags allBags subBagId)) bag

[<EntryPoint>]
let main argv =
    let rules = Helpers.readLines "input.txt"

    let bagMap = Solver.getBagMap rules
    let allContainers = Solver.getBagContainers bagMap rules

    let holdsBag = Solver.holdsBag allContainers bagMap.["shiny gold"]
    let canHoldCount =
        allContainers |> Map.toSeq |> Seq.map snd |> Seq.filter holdsBag |> Seq.length
    printfn "%i bags can hold shiny gold" canHoldCount

    let holdsCount =
        Solver.countBags allContainers bagMap.["shiny gold"]
    printfn "shiny gold must contain %i bags" holdsCount

    0