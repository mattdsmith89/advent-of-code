// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

module Helpers =
    let readLines (filePath: string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

module Solver =
    let findWeaknessKey preambleLength (input: int64 list) =
        let valid (p: int64 list) x =
            let pairs = 
                [for i = 0 to p.Length - 1 do
                    for j = 0 to p.Length - 1 do
                        if i <> j && p.[i] + p.[j] = x then (p.[i], p.[j])]
            not pairs.IsEmpty

        let rec findWeaknessKeyLoop (list: int64 list) =
            match list |> List.length with
            | x when x <= preambleLength -> failwith "no weakness key found"
            | _ ->
                let (currentPreamble, rest) = List.splitAt preambleLength list
                let validate = valid currentPreamble
                match validate rest.Head with
                | true -> findWeaknessKeyLoop list.Tail
                | false -> rest.Head
        findWeaknessKeyLoop input

    let findWeakness key (input: int64 list) =
        let rec tryGetSumRange key count (list: int64 list) =
            let values = list |> List.splitAt count |> fst
            match values |> List.sum with
            | x when x < key -> tryGetSumRange key (count + 1) list
            | x when x = key -> Some(values)
            | _ -> None
        
        let rec findWeaknessLoop key (list: int64 list) =
            match list with
            | [] -> failwith "no weakness found"
            | _::tail ->
                match tryGetSumRange key 0 list with
                | None -> findWeaknessLoop key tail
                | Some x ->
                    let min = List.min x
                    let max = List.max x
                    min + max

        findWeaknessLoop key input
            
[<EntryPoint>]
let main argv =
    let input = Helpers.readLines "input.txt" |> Seq.map int64 |> List.ofSeq

    let weaknessKey = Solver.findWeaknessKey 25 input
    printfn "weakness key %i" weaknessKey

    let weakness = Solver.findWeakness weaknessKey input
    printfn "weakness %i" weakness

    0 // return an integer exit code