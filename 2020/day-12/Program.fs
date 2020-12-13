// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

module Helpers =
    let readLines (filePath: string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

type Facing = N | S | E | W
type Turn = L | R

let valueToInt (value: char list) =
    System.String(value |> Array.ofList) |> int

let turn f d degrees =
    match degrees with
    | 90 ->
        match d with
        | R ->
            match f with
            | N -> E
            | E -> S
            | S -> W
            | W -> N
        | L ->
            match f with
            | N -> W
            | E -> N
            | S -> E
            | W -> S
    | 180 ->
        match f with
            | N -> S
            | E -> W
            | S -> N
            | W -> E
    | 270 ->
        match d with
        | R ->
            match f with
            | N -> W
            | E -> N
            | S -> E
            | W -> S
        | L ->
            match f with
            | N -> E
            | E -> S
            | S -> W
            | W -> N
    | _ -> f

let folder (e, n, f) elem =
    match elem with
    | action::value -> 
        match action with
        | 'N' -> (e, n + valueToInt value, f)
        | 'S' -> (e, n - valueToInt value, f)
        | 'E' -> (e + valueToInt value, n, f)
        | 'W' -> (e - valueToInt value, n, f)
        | 'L' -> (e, n, turn f L (valueToInt value))
        | 'R' -> (e, n, turn f R (valueToInt value))
        | 'F' -> 
            match f with
            | N -> (e, n + valueToInt value, f)
            | S -> (e, n - valueToInt value, f)
            | E -> (e + valueToInt value, n, f)
            | W -> (e - valueToInt value, n, f)
        | _ -> (e, n, f)
    | _ -> (e, n, f)

let turn2 d degrees (e, n) =
    match d with
    | L ->
        match degrees with
        | 90 -> (-1 * n, e)
        | 180 -> (-1 * e, -1 * n)
        | 270 -> (n, -1 * e)
        | _ -> (e, n)
    | R ->
        match degrees with
        | 90 -> (n,-1 * e)
        | 180 -> (-1 * e, -1 * n)
        | 270 -> (-1 * n, e)
        | _ -> (e, n)

let folder2 (e, n, wpe, wpn) elem =
    match elem with
    | action::value -> 
        match action with
        | 'N' -> (e, n, wpe, wpn + valueToInt value)
        | 'S' -> (e, n, wpe, wpn - valueToInt value)
        | 'E' -> (e, n, wpe + valueToInt value, wpn)
        | 'W' -> (e, n, wpe - valueToInt value, wpn)
        | 'L' -> 
            let (wpe', wpn') = turn2 L (valueToInt value) (wpe, wpn)
            (e, n, wpe', wpn')
        | 'R' -> 
            let (wpe', wpn') = turn2 R (valueToInt value) (wpe, wpn)
            (e, n, wpe', wpn')
        | 'F' -> (e + (valueToInt value * wpe), n + (valueToInt value * wpn), wpe, wpn)
        | _ -> (e, n, wpe, wpn)
    | _ -> (e, n, wpe, wpn)


let manhattan (x, y, _) =
    abs x + abs y

let manhattan2 (x, y, _, _) =
    abs x + abs y

[<EntryPoint>]
let main argv =
    let input = Helpers.readLines "input.txt" |> Seq.map Seq.toList

    let position = input |> Seq.fold folder (0, 0, E)
    printfn "manhattan %i" (manhattan position)

    let position2 = input |> Seq.fold folder2 (0, 0, 10, 1)
    printfn "manhattan2 %i" (manhattan2 position2)

    0 // return an integer exit code