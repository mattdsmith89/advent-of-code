// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO
open System.Text.RegularExpressions

module Helpers =
    let readLines (filePath: string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

    let (|Regex|_|) pattern input = 
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    type Policy = { Min: int; Max: int; Char: char; }

    let count x = Seq.filter ((=) x) >> Seq.length

    let parseInput string =
        match string with
        | Regex @"([\d]*)-([\d]*) ([\w]): ([\w]*)" [ min ; max; character; password ] 
            -> Some ({ Min=int min; Max=int max; Char=char character; }, password)
        | _ -> None

module PartOne =
    open Helpers
    let validatePassword input =
        let parsed = parseInput input
        match parsed with
        | Some((policy, password)) ->
            let charCount = password |> count policy.Char
            charCount >= policy.Min && charCount <= policy.Max
        | None -> false

module PartTwo =
    open Helpers
    let validatePassword input =
        let parsed = parseInput input
        match parsed with
        | Some((policy, password)) ->
            let minValid = password.[policy.Min - 1] = policy.Char
            let maxValid = password.[policy.Max - 1] = policy.Char
            minValid <> maxValid 
        | None -> false


[<EntryPoint>]
let main argv =
    let input = Helpers.readLines "input.txt" 
    
    let part1result = 
        input
        |> Seq.filter PartOne.validatePassword
        |> Seq.length
    
    let part2result = 
        input
        |> Seq.filter PartTwo.validatePassword
        |> Seq.length

    printfn "Part one result: %i valid passwords" part1result
    printfn "Part two result: %i valid passwords" part2result
    0 // return an integer exit code