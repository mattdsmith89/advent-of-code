// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

module Helpers =
    let readLines (filePath: string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

module Solver =
    let binarySearch (range: List<int>) (input: char) =
        let (front, back) = List.splitAt (range.Length / 2) range
        match input with
        | 'F' -> front
        | 'B' -> back
        | 'L' -> front
        | 'R' -> back
        | _ -> range
    
    let rec parsePartitions (partitions: string) range =
        let newRange = binarySearch range partitions.[0]
        match partitions.Length with
        | 1 -> newRange
        | _ -> parsePartitions partitions.[1..] newRange

    let parseBoardingPass (input: string) =
        let rowPartitions = input.[..6]
        let colPartitions = input.[7..]
        let rowRange = [ 0 .. 127 ]
        let colRange = [ 0 .. 7 ]
        let row = (parsePartitions rowPartitions rowRange).Head
        let col = (parsePartitions colPartitions colRange).Head
        row * 8 + col

[<EntryPoint>]
let main argv =
    let input = Helpers.readLines "input.txt"

    let seatIds = input |> Seq.map Solver.parseBoardingPass

    let minSeat = Seq.min seatIds
    let maxSeat = Seq.max seatIds
    printfn "highest id: %i" maxSeat

    let allSeatsSet = Set.ofList [ minSeat .. maxSeat ]
    let occupiedSeatsSet = Set.ofSeq seatIds
    let candidateSeats = Set.difference allSeatsSet occupiedSeatsSet
    for i in candidateSeats do printfn "my seat? %i" i

    0 // return an integer exit code