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

let parseBuses2 (input: string) =
    input.Split(',') 
    |> Array.mapi (fun i x -> (x, int64 i)) 
    |> Array.choose (fun (x, i) -> if x = "x" then None else Some (int64 x, (int64 x) - i))
    |> List.ofArray

let extgcd a b =
    let rec inner (r'', s'', t'') (r', s', t') =
        let step () =
            let q = r'' / r'
            let r = r'' - q*r'
            let s = s'' - q*s'
            let t = t'' - q*t'
            (r, s, t)
        if r' = 0L then (r'', s'', t'')
        else inner (r', s', t') (step())
    inner (a, 1L, 0L) (b, 0L, 1L)

let chinese (pairs: (int64 * int64) list) =
    let n = pairs |> List.map fst |> List.reduce ( * )
    let ays = pairs |> List.map snd
    let ns = pairs |> List.map fst
    let ys = ns |> List.map (fun x -> n / x)
    let zs = List.map2 extgcd ns ys |> List.map (fun (_, _, b) -> b)
    let x = List.map3 (fun a y z -> a * y * z) ays ys zs |> List.sum 
    ((x % n) + n) % n

[<EntryPoint>]
let main argv =
    let input = Helpers.readLines "input.txt" |> List.ofSeq
    let startTimestamp = input.Head |> int
    let buses = parseBuses input.[1]
    let (busTime, busId) = findBus startTimestamp buses
    printfn "result1 %i" ((busTime - startTimestamp) * busId)
    let result2 = chinese (parseBuses2 input.[1])
    printfn "result2 %i" result2
    0 // return an integer exit code