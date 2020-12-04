// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO
open System.Text.RegularExpressions

module Helpers =
    let readLines (filePath: string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

module Solver =
    let (|Regex|_|) pattern input = 
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let sortInput =
        let folder acc elem =
            if System.String.IsNullOrWhiteSpace elem
            then elem :: acc
            else 
                match acc with
                | head::tail -> head + " " + elem :: tail
                | [] -> [elem]
        Seq.fold folder []
    
    let isHexChar c =
        System.Char.IsDigit c 
        ||
        match c with
        |'a'|'b'|'c'|'d'|'e'|'f'|'A'|'B'|'C'|'D'|'E'|'F' -> true
        | _ -> false

    let isHex (input: string) =
        input.[0] = '#' 
        && input.[1..].Length = 6
        && List.forall isHexChar (List.ofSeq input.[1..])

    type CmOrInch =
    | Cm
    | Inch
    | Unknown

    type Passport = { 
        BirthYear: int; 
        IssueYear: int;
        ExpirationYear: int; 
        Height: int * CmOrInch; 
        HairColor: string; 
        EyeColor: string; 
        PassportId: string; 
        CountryId: string option; 
    }

    let getStringValue pattern input =
        match input with
        | Regex pattern [ value ] -> Some(value)
        | _ -> None

    let getIntValue pattern input =
        match getStringValue pattern input with
        | Some value -> Some(int value)
        | _ -> None

    let getBirthYear =
        getIntValue @"(?>byr:([\w]*))"

    let getIssueYear =
        getIntValue @"(?>iyr:([\w]*))"

    let getExpirationYear =
        getIntValue @"(?>eyr:([\w]*))"

    let getHeight input =
        let extract = getStringValue @"(?>hgt:([\w]*))" input
        match extract with
            | Some x -> 
                match x with
                | Regex @"([\d]*)([\w]*)" [ height; measure; ] -> 
                    let (success, parsedHeight) = System.Int32.TryParse height
                    if success then
                        match measure with
                        | "cm" -> Some((parsedHeight, Cm))
                        | "in" -> Some((parsedHeight, Inch))
                        | _ -> Some((parsedHeight, Unknown))
                    else None
                | _ -> None
            | None -> None

    let getHairColor =
        getStringValue @"(?>hcl:(#?[\w]*))"

    let getEyeColor =
        getStringValue @"(?>ecl:([\w]*))"

    let getPassportId =
        getStringValue @"(?>pid:([\w]*))"

    let getCountryId =
        getStringValue @"(?>cid:([\w]*))"

    let tryParse input =
        match getBirthYear input with
        | Some byr -> 
            match getIssueYear input with
            | Some iyr -> 
                match getExpirationYear input with
                | Some eyr -> 
                    match getHeight input with 
                    | Some hgt ->
                        match getHairColor input with
                        | Some hcl ->
                            match getEyeColor input with
                            | Some ecl ->
                                match getPassportId input with
                                | Some pid ->
                                    let passport = { 
                                        BirthYear=byr; 
                                        IssueYear=iyr; 
                                        ExpirationYear=eyr;
                                        Height=hgt;
                                        HairColor=hcl;
                                        EyeColor=ecl;
                                        PassportId=pid;
                                        CountryId=getCountryId input;
                                    }
                                    Some(passport)
                                | None -> None
                            | None -> None
                        | None -> None
                    | None -> None
                | None -> None
            | None -> None
        | None -> None

    let validatePassport input =
        match input with
            | { BirthYear=byr } -> byr >= 1920 && byr <= 2002
        && match input with
            | { IssueYear=iyr } -> iyr >= 2010 && iyr <= 2020
        && match input with
            | { ExpirationYear=eyr } -> eyr >= 2020 && eyr <= 2030
        && match input with
            | { Height=(height, measure) } ->
            match measure with
            | Cm -> height >=150 && height <= 193
            | Inch -> height >= 59 && height <= 76
            | Unknown -> false
        && match input with
            | { HairColor=hcl } -> isHex hcl
        && match input with
            | { EyeColor=ecl } ->
            match ecl with
            |"amb"|"blu"|"brn"|"gry"|"grn"|"hzl"|"oth" -> true
            |_ -> false
        && match input with
            | { PassportId=pid } -> 
            pid.Length = 9 && List.forall System.Char.IsDigit (List.ofSeq pid)

[<EntryPoint>]
let main argv =
    let input = Helpers.readLines "input.txt"

    let parsed = 
        input
        |> Solver.sortInput
        |> List.map Solver.tryParse
    
    let parseable =
        parsed
        |> List.filter (fun passport ->
            match passport with 
            | Some _ -> true
            | _ -> false)
        |> List.length

    printfn "%i parseable passports" parseable

    let valid =
        parsed
        |> List.filter (fun passport -> 
            match passport with
            | Some p -> Solver.validatePassport p
            | _ -> false)

    printfn "%i valid passports" (valid |> List.length)

    0 // return an integer exit code