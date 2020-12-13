// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

module Helpers =
    let readLines (filePath: string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

let countOccupiedNeighbors (grid: char list list) (x, y) =
    let xs = [x-1;x;x+1] |> List.filter (fun a -> a >= 0 && a < grid.[y].Length)
    let ys = [y-1;y;y+1] |> List.filter (fun a -> a >= 0 && a < grid.Length)
    [for i in xs do
        for j in ys do
            if not (j = y && i = x) && grid.[j].[i] = '#' then 1] |> List.sum

let countOccupiedInSight (grid: char list list) (x, y) =
    let directions = [(-1,-1); (-1,0); (-1,1); (0,-1); (0,1); (1,-1); (1,0); (1,1)]
    let rec checkPosition (grid: char list list) (x, y) (dx, dy) =
        let (x', y') = (x + dx, y + dy)
        if not (x' >= 0 && x' < grid.[0].Length && y' >= 0 && y' < grid.Length) then 0
        else
            match grid.[y'].[x'] with
            | '#' -> 1
            | 'L' -> 0
            | _ -> checkPosition grid (x', y') (dx, dy)
    directions |> List.sumBy (checkPosition grid (x,y))

let printGrid input =
    for line in input do
        for char in line do printf "%c" char
        printf "\n"

let applyRules (grid: char list list) =
    grid |> List.mapi (
        fun y xs -> 
            xs |> List.mapi (
                fun x char -> 
                    match char with
                    | 'L' -> if countOccupiedNeighbors grid (x, y) = 0 then '#' else 'L'
                    | '#' -> if countOccupiedNeighbors grid (x, y) >= 4 then 'L' else '#'
                    | _ -> char))

let applyLosRules (grid: char list list) =
    grid |> List.mapi (
        fun y xs -> 
            xs |> List.mapi (
                fun x char -> 
                    match char with
                    | 'L' -> if countOccupiedInSight grid (x, y) = 0 then '#' else 'L'
                    | '#' -> if countOccupiedInSight grid (x, y) >= 5 then 'L' else '#'
                    | _ -> char))

let countAllOccupied (grid: char list list) =
    grid |> List.sumBy (fun x -> x |> List.sumBy (fun y -> if y = '#' then 1 else 0))

let run (grid: char list list) =
    let rec runLoop (grid: char list list) =
        let nextGrid = applyRules grid
        match nextGrid = grid with
        | true -> countAllOccupied nextGrid
        | _ -> runLoop nextGrid
    runLoop grid

let run2 (grid: char list list) =
    let rec runLoop (grid: char list list) =
        let nextGrid = applyLosRules grid
        match nextGrid = grid with
        | true -> countAllOccupied nextGrid
        | _ -> runLoop nextGrid
    runLoop grid

[<EntryPoint>]
let main argv =
    let input = Helpers.readLines "input.txt" |> Seq.map Seq.toList |> Seq.toList

    printfn "run1 %i" (run input)
    printfn "run2 %i" (run2 input)

    0 // return an integer exit code