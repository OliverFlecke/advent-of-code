#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net5.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Utils
open System
open System.Text.RegularExpressions

let data = input 2020 4
let testData = testInput 2020 4

let parse (str: string) =
    str.Split("\n\n")
    |> Seq.map (fun s -> s.Replace("\n", " "))

let fields =
    [ "byr"
      "iyr"
      "eyr"
      "hgt"
      "hcl"
      "ecl"
      "pid" ]

let validate (str: string) =
    Seq.forall (fun (f: string) -> str.Contains(f)) fields

let solve =
    parse >> Seq.filter validate >> Seq.length

printfn "Solution for test data %d"
<| solve testData

submit 2020 4 Level.One <| solve data

// Level 2
let invalid = """eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007"""

let valid = """pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"""

let validateField (key, value) =
    match key with
    | "byr" -> int value |> (fun v -> 1920 <= v && v <= 2002)
    | "iyr" -> int value |> (fun v -> 2010 <= v && v <= 2020)
    | "eyr" -> int value |> (fun v -> 2020 <= v && v <= 2030)
    | "hgt" ->
        let m =
            Regex.Match(value, @"((?<cm>\d{3})cm)|((?<in>\d{2})in)")

        if not m.Success then
            false
        else if not <| String.IsNullOrEmpty m.Groups.["cm"].Value then
            m.Groups.["cm"].Value
            |> int
            |> fun h -> 150 <= h && h <= 193
        else
            m.Groups.["in"].Value
            |> int
            |> fun h -> 59 <= h && h <= 76
    | "hcl" -> Regex.IsMatch(value, @"#[\da-f]{6}")
    | "ecl" -> Regex.IsMatch(value, @"amb|blu|brn|gry|grn|hzl|oth")
    | "pid" -> Regex.IsMatch(value, @"^\d{9}$")
    | "cid" -> true
    | _ -> failwith $"{key} is not supported"

let validateFields = Seq.forall validateField

let parseField =
    function
    | [| key; value |] -> (key, value)
    | x -> failwith $"unable to parse field {string x}"

let parse2 =
    Seq.map
        (splitSpaces
         >> Seq.map (fun s -> s.Split(":") |> parseField))

let filter2 =
    Seq.filter validate
    >> parse2
    >> Seq.filter validateFields

let solve2 = parse >> filter2 >> Seq.length

// printfn "%b" <| validateField ("byr", "2002")
// printfn "%b" (not <| validateField ("byr", "2003"))
// printfn "%b" <| validateField ("hgt", "60in")
// printfn "%b" <| validateField ("hgt", "190cm")
// printfn "%b" (not <| validateField ("hgt", "190in"))
// printfn "%b" (not <| validateField ("hgt", "190"))
// printfn "%b" (validateField ("hcl", "#123abc"))
// printfn "%b" (not <| validateField ("hcl", "#123abz"))
// printfn "%b" (not <| validateField ("hcl", "123abc"))

// printfn "%b" (validateField ("ecl", "brn"))
// printfn "%b" (not <| validateField ("ecl", "wat"))

// printfn "%b" (validateField ("pid", "000000001"))
// printfn "%b" (not <| validateField ("pid", "0123456789"))

// solve2 invalid
// |> printfn "Number if valid in the invalid list %d"

// solve2 valid |> printfn "Valid passed %d"

submit 2020 4 Level.Two <| solve2 data
