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

[<EntryPoint>]
let main argv =
    let (testA, testB, testValue) = testResult
    let (realA, realB, realValue) = realResult
    printfn "Part 1 Test Result is %i from %i and %i" testValue testA testB
    printfn "Part 1 Real Result is %i from %i and %i" realValue realA realB
    0 // return an integer exit code