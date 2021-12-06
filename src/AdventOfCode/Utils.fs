namespace AdventOfCode

open System.Security.Cryptography
open System.Text
open System.Text.RegularExpressions
open AdventOfCode.Core

module Utils =
    let splitSpaces (str: string) = str.Split(" ")
    let splitLines (str: string) = str.Split("\n")
    let splitComma (str: string) = str.Split(",")
    let trim (str: string) = str.Trim()
    let toLower (str: string) = str.ToLowerInvariant()

    let ints (s: string) = s.Trim() |> splitLines |> Seq.map int

    let count c = Seq.filter ((=) c) >> Seq.length
    let modulo m n = ((n % m) + m) % m
    let manhattan (x, y) = abs x + abs y

    let print x = printfn "%A" x

    let testSolution (level: Level) expected actual =
        if expected = actual then
            pSuccess $"Level {level} passed"
        else
            pError $"Level {level} failed. Expected {expected}, got {actual}"

    let rec private combinationsImpl acc size set =
        seq {
            match size, set with
            | n, x :: xs ->
                if n > 0 then
                    yield! combinationsImpl (x :: acc) (n - 1) xs

                if n >= 0 then
                    yield! combinationsImpl acc n xs
            | 0, [] -> yield acc
            | _, [] -> ()
        }

    let combinations n set = combinationsImpl [] n set

    let (|ReMatch|_|) pattern input =
        if isNull input then
            None
        else
            let m =
                Regex.Match(input, pattern, RegexOptions.Compiled)

            if m.Success then
                Some [ for x in m.Groups -> x ]
            else
                None

    let (|Int|_|) (str: string) =
        match System.Int32.TryParse str with
        | true, int -> Some int
        | _ -> None

    let rec tails =
        function
        | [] -> []
        | _ :: xs -> xs :: tails xs

    let filterOption (ls: seq<'a option>) =
        seq {
            for x in ls do
                match x with
                | Some item -> yield item
                | None -> ()
        }

    let mapOption f = Seq.map f >> filterOption

    let directions =
        seq {
            for x = -1 to 1 do
                for y = -1 to 1 do
                    if x <> 0 || y <> 0 then yield (x, y)
        }

    let take amount (list: seq<'a>) =
        let length = Seq.length list

        seq {
            for i in 0 .. amount .. length - 1 do
                yield list |> Seq.skip i |> Seq.take amount
        }

    let transpose array =
        array |> Array2D.mapi (fun x y _ -> array.[y, x])

    let md5 (message: string) =
        use hasher = MD5.Create()

        message
        |> Encoding.ASCII.GetBytes
        |> hasher.ComputeHash
        |> Seq.map (fun c -> c.ToString("X2"))
        |> Seq.reduce (+)