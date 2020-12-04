// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

module Helpers =
    let readLines (filePath: string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

module Solver =
    let folder right down acc (elem: string) =
        let width = elem.Length
        let (row, collisions) = acc
        if row % down = 0 then
            let position = (row / down) * right % width
            if elem.[position] = '.' 
            then (row + 1, collisions) 
            else (row + 1, collisions + 1)
        else (row + 1, collisions)

    let countCollisions lines instruction  =
        let (right, down) = instruction
        let folderWithInstruction = folder right down
        let (_, collisions) = Seq.fold folderWithInstruction (0, 0) lines
        collisions
        |> int64

[<EntryPoint>]
let main argv =
    let lines = Helpers.readLines "input.txt"

    let instruction = (3,1)
    let part1result =
        instruction |> (Solver.countCollisions lines)

    let instructions = [(1,1); (3,1); (5,1); (7,1); (1,2)]

    let part2result =
        instructions
        |> List.map (Solver.countCollisions lines)
        |> List.reduce ( * )

    printfn "Part 1 result: %i" part1result
    printfn "Part 2 result: %i" part2result

    0 // return an integer exit code