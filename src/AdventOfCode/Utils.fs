namespace AdventOfCode

open System.Security.Cryptography
open System.Text
open System.Text.RegularExpressions
open AdventOfCode.Core

module Utils =
    // String helpers
    let splitSpaces (str: string) = str.Split(" ")
    let splitLines (str: string) = str.Split("\n")
    let splitComma (str: string) = str.Split(",")
    let trim (str: string) = str.Trim()
    let toLower (str: string) = str.ToLowerInvariant()

    let ints (s: string) = s.Trim() |> splitLines |> Seq.map int

    // List helpers
    module Seq =
        /// <summary>Count the number of an element in a sequence.</summary>
        let count c = Seq.filter ((=) c) >> Seq.length

        /// <summary>Perform a side-effect for each element in the sequenc.</summary>
        let perform f sequence =
            Seq.iter f sequence
            sequence

        let performi f sequence =
            Seq.iteri f sequence
            sequence

        /// <summary>Compute the frequency map of each element in the list.</summary>
        let frequency (s: seq<'a>) =
            s
            |> Seq.groupBy id
            |> Seq.map (fun (x, xs) -> x, Seq.length xs)

        let takeWindow amount (list: seq<'a>) =
            let length = Seq.length list

            seq {
                for i in 0 .. amount .. length - 1 do
                    yield list |> Seq.skip i |> Seq.truncate amount
            }

        /// <summary>Find the most common element in an sequence.</summary>
        let mostCommon xs =
            xs
            |> Seq.groupBy id
            |> Seq.maxBy (snd >> Seq.length)
            |> fst

        /// <summary>Find the least common element in a sequence.</summary>
        let leastCommon xs =
            xs
            |> Seq.groupBy id
            |> Seq.minBy (snd >> Seq.length)
            |> fst

        /// <summary>Sort a sequence of elements by how common they are.</summary>
        let sortByCommon xs =
            xs
            |> Seq.groupBy id
            |> Seq.sortBy (snd >> Seq.length)
            |> Seq.map fst

        /// <summary>Get all pairs in the list.</summary>
        /// <remarks>Note that both x,y and y,x is returned.</remarks>
        let pairs (xs: 'a seq) =
            let ls = List.ofSeq xs

            seq {
                for x in 0 .. ls.Length - 1 do
                    for y in 0 .. ls.Length - 1 do
                        if x <> y then yield ls.[x], ls.[y]
            }

    module List =
        let pairs (ls: 'a list) =
            seq {
                for x in 0 .. ls.Length - 1 do
                    for y in 0 .. ls.Length - 1 do
                        if x <> y then yield ls.[x], ls.[y]
            }

        let pairsWithoutOrder (ls: 'a list) =
            seq {
                for x in 0 .. ls.Length - 1 do
                    for y in x + 1 .. ls.Length - 1 do
                        yield ls.[x], ls.[y]
            }

    // General functional helpers
    let flip f x y = f y x
    let curry f a b = f (a, b)
    let uncurry f (a, b) = f a b

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

    /// <summary>Use regex in a pattern match.</summary>
    /// <param name="pattern">Regex pattern to match.</param>
    /// <param name="input">The string to match.</param>
    /// <returns>List of groups in the regex, if successful.</returns>
    /// <example>
    /// Can for example be used to match and extract numbers from a string.
    ///
    /// ```
    /// match someString with
    /// | ReMatch "int (\d+)" [ amount ] -> amount
    /// | ReMatch "add (\d+) (\d+)" [ a; b ] -> a + b
    /// ```
    /// </example>
    let (|ReMatch|_|) pattern input =
        if isNull input then
            None
        else
            let m =
                Regex.Match(input, pattern, RegexOptions.Compiled)

            if m.Success then
                Some(List.skip 1 <| [ for x in m.Groups -> x.Value ])
            else
                None

    /// <summary>Pattern match an string to a int.</summary>
    /// <example>
    /// ```
    /// match someString with
    /// | Int i -> printfn "someString is an int '%i'" i
    /// | _ -> printfn "someString '%s' is NOT an int" someString
    /// ```
    /// </example>
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

    /// <summary>Get a vector to each horizontally, vertically, and diagonally adjacent point.</summary>
    let directions =
        seq {
            for x = -1 to 1 do
                for y = -1 to 1 do
                    if x <> 0 || y <> 0 then yield (x, y)
        }

    let directionWithoutDiagonals =
        Seq.ofList [ (0, 1)
                     (1, 0)
                     (-1, 0)
                     (0, -1) ]

    /// <summary>Get all the neighbors inside the grid.</summary>
    let neighbors grid p =
        directions
        |> Seq.map (fun (x, y) -> x + fst p, y + snd p)
        |> Seq.filter (fun (x, y) ->
            0 <= x
            && x < Array2D.length1 grid
            && 0 <= y
            && y < Array2D.length2 grid)

    // Matrix operations
    let transpose (mtx: _ [,]) =
        Array2D.init (Array2D.length2 mtx) (Array2D.length1 mtx) (fun x y -> mtx.[y, x])

    let getRow row (matrx: 'a [,]) = matrx.[row..row, *] |> Seq.cast<'a>
    let getCol col (matrx: 'a [,]) = matrx.[*, col..col] |> Seq.cast<'a>

    /// <summary>Calculate md5 hash of string.</summary>
    let md5 (message: string) =
        use hasher = MD5.Create()

        message
        |> Encoding.ASCII.GetBytes
        |> hasher.ComputeHash
        |> Seq.map (fun c -> c.ToString("X2"))
        |> Seq.reduce (+)


    // Math formulars

    /// <summary>Calculate the sum of numbers from 1 to n.</summary>
    /// <param name="n">Upper limit of the sum.</summary>
    /// <returns>Sum of all numbers from 1 to n.</returns>
    /// <example>```1 + 2 + 3 + ... + n```</example>
    let sumOfNumbers n = (n * (n + 1)) / 2

    let modulo m n = ((n % m) + m) % m
    let manhattan (x, y) = abs x + abs y
