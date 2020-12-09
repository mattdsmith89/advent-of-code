// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

module Helpers =
    let readLines (filePath: string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

module Solver =
    type Result =
        | Infinite
        | Normal
        | BadInstruction
        | UnknownOp

    let readInstructions =
        let rec readInstructionLoop (visited: int Set) acc next (instructions: string list) =
            if visited.Contains next then (acc, Infinite)
            else if next = instructions.Length then (acc, Normal)
            else if next < 0 || next > instructions.Length then (acc, BadInstruction)
            else
            let newVisited = visited.Add next
            let instruction = instructions.[next]
            let operation = instruction.[..2]
            let argument = int instruction.[4..]
            match operation with
            | "nop" -> readInstructionLoop newVisited acc (next + 1) instructions
            | "acc" -> readInstructionLoop newVisited (acc + argument) (next + 1) instructions
            | "jmp" -> readInstructionLoop newVisited acc (next + argument) instructions
            | _ -> (acc, UnknownOp)

        readInstructionLoop Set.empty 0 0

    let runTweaks =
        let swapOperation (instruction: string) =
            let operation = instruction.[..2]
            let argument = instruction.[4..]
            match operation with
            | "nop" -> "jmp " + argument
            | "jmp" -> "nop " + argument
            | _ -> instruction

        let swapInstruction next =
            List.mapi (fun i x -> if i = next then swapOperation x else x)

        let rec runTweaksLoop next (instructions: string list) =
            let tweakedInstructions = swapInstruction next instructions
            let result = readInstructions tweakedInstructions
            match result |> snd with
            | Infinite -> runTweaksLoop (next + 1) instructions
            | BadInstruction -> runTweaksLoop (next + 1) instructions
            | Normal -> result |> fst
            | UnknownOp -> failwith "instructions contained unknown operation" 

        runTweaksLoop 0

[<EntryPoint>]
let main argv =
    let instructions = Helpers.readLines "input.txt" |> Seq.toList
    
    let part1FinalAcc = Solver.readInstructions instructions |> fst
    printfn "final acc %i" part1FinalAcc

    let part2FinalAcc = Solver.runTweaks instructions
    printfn "final fixed acc %i" part2FinalAcc

    0 // return an integer exit code