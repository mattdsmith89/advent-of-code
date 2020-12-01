// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

module Helpers =
    let readLines (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

    let splitLine = fun (line: string) ->
        (line.Split '\n')
        |> List.ofArray

    let printPart1Result resultType values =
        let (a, b, result) = values
        printfn "Part 1 %s result: %i" resultType result
        printfn "%i and %i" a b

    let printPart2Result resultType values =
        let (a, b, c, result) = values
        printfn "Part 2 %s result: %i" resultType result
        printfn "%i, %i and %i" a b c

module PartOne =
    let folder head acc elem =
        if head + elem = 2020
        then (head, elem, head * elem)
        else acc

    let rec sumCheckMultiply list =
        match list with
        | head::tail ->
            let result = Seq.fold (folder head) (0, 0, 0) tail
            match result with
            | (0, 0, 0) -> sumCheckMultiply tail
            | _ -> result
        | [] -> (0, 0, 0)

module PartTwo =
    let innerFolder mainHead outerElem acc elem =
        if mainHead + outerElem + elem = 2020
        then (mainHead, outerElem, elem, mainHead * outerElem * elem)
        else acc

    let outerFolder mainHead list acc elem =
        match list with
        | _::tail -> Seq.fold (innerFolder mainHead elem) acc tail
        | [] -> (0, 0, 0, 0)

    let rec sumCheckMultiply list =
        match list with
        | head::tail ->
            let result = Seq.fold (outerFolder head tail) (0, 0, 0, 0) tail
            match result with
            | (0, 0, 0, 0) -> sumCheckMultiply tail
            | _ -> result
        | [] -> (0, 0, 0, 0)

[<EntryPoint>]
let main argv =
    
    let test = 
        "1721\n979\n366\n299\n675\n1456"
        |> Helpers.splitLine 
        |> List.map int

    let real = 
        Helpers.readLines "input.txt"
        |> Seq.map int 
        |> List.ofSeq

    Helpers.printPart1Result "test" (PartOne.sumCheckMultiply test)
    Helpers.printPart1Result "real" (PartOne.sumCheckMultiply real)

    printfn ""
    
    Helpers.printPart2Result "test" (PartTwo.sumCheckMultiply test)
    Helpers.printPart2Result "real" (PartTwo.sumCheckMultiply real)

    0 // return an integer exit code