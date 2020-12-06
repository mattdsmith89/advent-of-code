// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

module Helpers =
    let readLines (filePath: string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

module Solver =
    let parseGroups =
        let folder acc elem =
            if System.String.IsNullOrWhiteSpace elem
            then 
                []::acc
            else 
                match acc with
                | head::tail -> (elem :: head) :: tail
                | [] -> [[elem]]
        Seq.fold folder []  
    
    let whereAnyone = 
        Seq.sumBy (Seq.reduce (+) >> Set.ofSeq >> Set.count)

    let whereEveryone =
        Seq.sumBy (Seq.map Set.ofSeq >> Set.intersectMany >> Set.count)

[<EntryPoint>]
let main argv =
    let groups =
        Helpers.readLines "input.txt"
        |> Solver.parseGroups

    let anyone =
        groups |> Solver.whereAnyone
    printfn "sum where anyone: %i" anyone 

    let everyone =
        groups |> Solver.whereEveryone
    printfn "sum where everyone: %i" everyone
   
    0 // return an integer exit code