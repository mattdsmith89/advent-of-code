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

    let (testA, testB, testValue) = PartOne.sumCheckMultiply test 
    let (realA, realB, realValue) = PartOne.sumCheckMultiply real

    printfn "Part 1 Test Result is %i from %i and %i" testValue testA testB
    printfn "Part 1 Real Result is %i from %i and %i" realValue realA realB

    let (testA, testB, testC, testValue) = PartTwo.sumCheckMultiply test
    let (realA, realB, realC, realValue) = PartTwo.sumCheckMultiply real

    printfn "Part 2 Test Result is %i from %i, %i and %i" testValue testA testB testC
    printfn "Part 2 Real Result is %i from %i, %i and %i" realValue realA realB realC

    0 // return an integer exit code