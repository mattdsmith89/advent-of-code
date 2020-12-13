// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

module Helpers =
    let readLines (filePath: string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

let chooser timestamp busId =
    if timestamp % busId = 0 then Some (timestamp, busId) else None

let findBus timestamp buses =
    let rec findBusLoop timestamp buses =
        let thisChooser = chooser timestamp
        match buses |> List.choose thisChooser with
        | [x] -> x
        | _ -> findBusLoop (timestamp + 1) buses
    findBusLoop timestamp buses

let parseBuses (input: string) =
    input.Split(',') |> Array.filter (fun x -> x <> "x") |> Seq.map int |> List.ofSeq

[<EntryPoint>]
let main argv =
    let input = Helpers.readLines "input.txt" |> List.ofSeq
    let startTimestamp = input.Head |> int
    let buses = parseBuses input.[1]
    let (busTime, busId) = findBus startTimestamp buses
    printfn "result1 %i" ((busTime - startTimestamp) * busId)
    0 // return an integer exit code