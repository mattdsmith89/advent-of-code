// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let splitLine = fun (line: string) ->
    (line.Split '\n')
    |> List.ofArray

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

let innerFolder2 mainHead outerElem acc elem =
    if mainHead + outerElem + elem = 2020
    then (mainHead, outerElem, elem, mainHead * outerElem * elem)
    else acc


let outerFolder2 mainHead list acc elem =
    match list with
    | _::tail -> Seq.fold (innerFolder2 mainHead elem) acc tail
    | [] -> (0, 0, 0, 0)

let rec sumCheckMultiply2 list =
    match list with
    | head::tail ->
        let result = Seq.fold (outerFolder2 head tail) (0, 0, 0, 0) tail
        match result with
        | (0, 0, 0, 0) -> sumCheckMultiply2 tail
        | _ -> result
    | [] -> (0, 0, 0, 0)

let testInput = "1721
979
366
299
675
1456"

let parsedTest =
    testInput
    |> splitLine
    |> List.map int

let realInput = 
    readLines "input.txt"
    |> Seq.map int 
    |> List.ofSeq

let testResult = sumCheckMultiply parsedTest
let realResult = sumCheckMultiply realInput

let test2Result = sumCheckMultiply2 parsedTest
let real2Result = sumCheckMultiply2 realInput

[<EntryPoint>]
let main argv =
    let (testA, testB, testValue) = testResult
    let (realA, realB, realValue) = realResult
    printfn "Part 1 Test Result is %i from %i and %i" testValue testA testB
    printfn "Part 1 Real Result is %i from %i and %i" realValue realA realB

    let (testA, testB, testC, testValue) = test2Result
    let (realA, realB, realC, realValue) = real2Result
    printfn "Part 2 Test Result is %i from %i, %i and %i" testValue testA testB testC
    printfn "Part 2 Real Result is %i from %i, %i and %i" realValue realA realB realC
    0 // return an integer exit code