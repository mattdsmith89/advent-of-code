// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO
open System.Text.RegularExpressions

module Helpers =
    let readLines (filePath: string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

module Shared =
    let (|Regex|_|) pattern input = 
            let m = Regex.Match(input, pattern)
            if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
            else None

    let (|Prefix|_|) (p: string) (s: string) =
        if s.StartsWith(p) then
            Some(s.Substring(p.Length))
        else
            None

    let toBinary padding value =
        let rec toBinaryLoop acc value = 
            match value with
            | 0L | 1L -> value::acc
            | _ -> toBinaryLoop ((value % 2L)::acc) (value / 2L)
        let partResult = toBinaryLoop [] value
        let padding = 
            if partResult.Length = padding 
            then [] 
            else [ for _ = 0 to ((padding - 1) - partResult.Length) do 0L ]
        padding@partResult

    let toBinary36 = toBinary 36

    let toInt = List.rev >> List.mapi (fun i x -> (pown 2L i) * x) >> List.sum

    let initialMask = [ for _ in 1..36 -> 'X' ]

module Solver =
    open Shared

    let solvePart1 =
        let applyMask maskItem value =
            match maskItem with
            | 'X' -> value
            | _ -> int64 maskItem - int64 '0'

        let updateMem (mem: Map<int,int64>) mask update =
            match update with
            | Regex @"\[([\d]*)\] = ([\d]*)" [ address; value; ] ->
                let binaryValue = toBinary36 (int64 value)
                let maskedValue = List.map2 applyMask mask binaryValue |> toInt
                mem.Add (int address, maskedValue)
            | _ -> mem

        let folder (mask, mem) elem =
            match elem with
            | Prefix "mask = " mask' -> (mask' |> Seq.toList, mem)
            | Prefix "mem" update -> (mask, updateMem mem mask update)
            | _ -> (mask, mem)
    
        Seq.fold folder (initialMask, Map.empty) 
        >> snd 
        >> Seq.sumBy (fun x -> x.Value)
    
    let solvePart2 =
        let applyMask mask bits =
            let rec mergeMaskLoop mask bits mask' =
                match mask with
                | [] -> mask'
                | x::xs ->
                    match x with
                    | 'X' ->
                        match bits with
                        | [] -> mask'
                        | bit::bits' -> mergeMaskLoop xs bits' (bit::mask')
                    | _ -> mergeMaskLoop xs bits ((int64 x - int64 '0')::mask')
            mergeMaskLoop mask bits []

        let mergeMask maskItem addressItem =
            match maskItem with
            | '0' -> 
                match addressItem with
                | 0L -> '0'
                | 1L -> '1'
                | _ -> failwith "unexpected address item"
            | _ -> maskItem

        let updateMem (mem: Map<int64,int64>) mask update =
            match update with
            | Regex @"\[([\d]*)\] = ([\d]*)" [ address; value; ] ->
                let maskedAddress = int64 address |> toBinary36 |> List.map2 mergeMask mask
                let n = mask |> List.sumBy (fun x -> if x = 'X' then 1 else 0)
                let maskBits = [ 0 .. pown 2 n ] |> List.map (int64 >> (toBinary n))
                let addresses = maskBits |> List.map (applyMask maskedAddress >> toInt)
                List.fold (fun (m: Map<int64,int64>) a -> m.Add (a, int64 value)) mem addresses
            |_ -> mem
            
        let folder (mask, mem) elem =
            match elem with
            | Prefix "mask = " mask' -> (mask' |> Seq.toList, mem)
            | Prefix "mem" update -> (mask, updateMem mem mask update)
            | _ -> (mask, mem)
        
        Seq.fold folder (initialMask, Map.empty) 
        >> snd 
        >> Seq.sumBy (fun x -> x.Value)

[<EntryPoint>]
let main argv =
    let input = Helpers.readLines "input.txt"
    let result1 = Solver.solvePart1 input
    printfn "result1 %i" result1
    let result2 = Solver.solvePart2 input
    printfn "result2 %i" result2
    0 // return an integer exit code