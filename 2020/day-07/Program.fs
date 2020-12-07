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
            |> Seq.choose (fun x ->
                if x.StartsWith("no") then None
                else match x with
                        | Regex @"([\d]*) ([\w]* [\w]*)" [ count; bag; ] -> 
                            Some (int count, bagMap.[bag])
                        | _ -> None)
        (containerId, parsedContents)

    let getBagContents (bagMap: Map<string,int>) (input: string seq) =
        let containerMapperWithMap = containerMapper bagMap
        input
        |> Seq.map containerMapperWithMap
        |> Map

    let rec holdsBag (allBagsWithContents: Map<int, (int * int) seq>) bagToCheck (currentBag: (int * int) seq) =
        let innerBags = currentBag |> Seq.map snd
        let checkNextBag nextBagId =
            let nextBag = allBagsWithContents.[nextBagId]
            holdsBag allBagsWithContents bagToCheck nextBag
        if Seq.contains bagToCheck innerBags then true
        else Seq.exists checkNextBag innerBags

    let rec countBags (allBagsWithContents: Map<int, (int * int) seq>) bagId =
        let bag = allBagsWithContents.[bagId]
        let sumInner innerBag =
            let subBagCount = innerBag |> fst
            let subBagId = innerBag |> snd
            let subBagInnerCount =
                countBags allBagsWithContents subBagId
            subBagCount + subBagCount * subBagInnerCount
        Seq.sumBy sumInner bag

[<EntryPoint>]
let main argv =
    let rules = Helpers.readLines "input.txt"

    let bagMap = Solver.getBagMap rules
    let bagsWithContents = Solver.getBagContents bagMap rules

    let holdsBag = Solver.holdsBag bagsWithContents bagMap.["shiny gold"]
    let canHoldCount =
        bagsWithContents |> Map.toSeq |> Seq.map snd |> Seq.filter holdsBag |> Seq.length
    printfn "%i bags can hold shiny gold" canHoldCount

    let holdsCount =
        Solver.countBags bagsWithContents bagMap.["shiny gold"]
    printfn "shiny gold must contain %i bags" holdsCount

    0