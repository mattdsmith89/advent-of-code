// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

module Helpers =
    let readLines (filePath: string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

[<EntryPoint>]
let main argv =
    
    let x = Helpers.readLines "input.txt" |> Seq.map int

    let p1 =
       [ for i in x do 
         for j in x do
         if i + j = 2020 then i * j ].Head

    let p2 =
        [ for i in x do
          for j in x do
          for k in x do
          if i + j + k = 2020 then i * j * k ].Head

    printfn "result 1 %i" p1
    printfn "result 2 %i" p2
    


    0 // return an integer exit code