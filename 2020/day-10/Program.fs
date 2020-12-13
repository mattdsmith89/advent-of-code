open System.IO
open System.Collections.Generic

module Helpers =
    let readLines (filePath: string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

module Solver =
    let getJolts (input: string seq) =
        let jolts = input |> Seq.map int |> Seq.toList
        let deviceJoltage = Seq.max jolts + 3
        [0; deviceJoltage] @ jolts |> List.sort

    type JoltageGap = JoltageGap of int

    let countGaps (input: int seq) =
        let chooser = (fun (a, b) -> 
            match b - a with
            | 3 -> Some(JoltageGap 3)
            | 2 -> Some(JoltageGap 2)
            | 1 -> Some(JoltageGap 1)
            | _ -> None)
        let gaps = input |> Seq.pairwise |> Seq.choose chooser
        let mapper gaps x =
            let count = 
                gaps 
                |> Seq.filter (fun y -> y = JoltageGap x) 
                |> Seq.length
            (JoltageGap x, count)
        [1;2;3] |> List.map (mapper gaps)

    let memoize f =
        let cache = new Dictionary<_,_>()
        (fun x ->
            let succ, v = cache.TryGetValue(x)
            if succ then v else
                let v = f(x)
                cache.Add(x, v)
                v)

    let countPaths jolts =
        let rec countPathsLoop =
            memoize (fun j ->
            if j = List.last jolts then 1 |> int64 else
                let x = 0 |> int64
                let x' = 
                    if List.contains (j + 1) jolts 
                    then (countPathsLoop (j + 1)) + x 
                    else x
                let x'' = 
                    if List.contains (j + 2) jolts 
                    then (countPathsLoop (j + 2)) + x' 
                    else x'
                let x''' = 
                    if List.contains (j + 3) jolts 
                    then (countPathsLoop (j + 3)) + x'' 
                    else x''
                x''')
        countPathsLoop 0

[<EntryPoint>]
let main argv =
    let input = Helpers.readLines "input.txt"

    let gaps = input |> Solver.getJolts |> Solver.countGaps
    for j in gaps do printfn "%A" j

    let paths = input |> Solver.getJolts |> Solver.countPaths
    printfn "paths %i" paths


    0 // return an integer exit code