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

let rec sumCheckMultiply list =
    match list with
    | head::tail ->
        let result = Seq.fold (fun acc elem -> if head + elem = 2020 then head * elem else acc) 0 tail
        match result with
        | 0 -> sumCheckMultiply tail
        | _ -> result
    | [] -> 0

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
    printfn "Test Result is %i" testResult
    printfn "Real Result is %i" realResult
    0 // return an integer exit code