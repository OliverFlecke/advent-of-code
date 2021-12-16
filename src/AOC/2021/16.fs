namespace AdventOfCode.Solutions

open System
open AdventOfCode
open AdventOfCode.Utils

[<AutoOpen>]
module private Day16 =
    let fromBinary (str: string) = Convert.ToInt32(str, 2)
    let fromHex (str: string) = Convert.ToInt32(str, 16)
    let toBinary (number: int) = Convert.ToString(number, 2).PadLeft(4, '0')

    let parse (str: string) =
        Seq.map (fun (c: char) -> string c |> fromHex |> toBinary) str
        |> String.Concat

    let parseHeader (str: string) =
        let version = fromBinary str.[..2]
        let id = fromBinary str.[3..5]
        version, id, str[6..]

    type Header = int * int
    type Node =
        | Literal of Header * string
        | Operator of Header * Node list

    let parseLiteral maxLength header (str: string) =
        printfn "parseLiteral %s" str
        let rec helper l (str: string) =
            if l = 0 then [], str
            else
                match str.[..4] |> List.ofSeq with
                | '0' :: number when number.Length = 4 ->
                    // printfn "Remainder '%s'" str.[5..]
                    number, str.[5..]
                | '1' :: number ->
                    let result, remainder = helper (l - 5) str.[5..]
                    List.append number result, remainder
                | _ -> failwith "UNREACHABLE"

        let result, remainder = helper maxLength str
        Literal (header, result |> String.Concat), remainder

    let rec parseOperator header (str: string) =
        printfn "parseOperator %s" str
        let parseEither length version id str =
            if id = 4
            then parseLiteral length (version, id) str
            else parseOperator (version, id) str

        if str.[0] = '0' then
            let mutable length = fromBinary str.[1..15]
            let mutable rest = str.[16..]
            let mutable ls = []

            while length > 0 do
                let version, id, str' = parseHeader rest
                let node, remainder = parseEither (length - 6) version id str'
                length <- length - (rest.Length - remainder.Length)
                ls <- node :: ls
                rest <- remainder

            Operator (header, List.rev ls), rest

        else
            let packets = str.[1..11]
            let numberOfSubPackets = fromBinary packets

            printfn "Subpackets: %i from: '%s' length %i" numberOfSubPackets packets packets.Length
            let mutable ls = []
            let mutable rest = str.[12..]
            for i in 1..numberOfSubPackets do
                let version, id, rest' = parseHeader rest
                let result = parseEither Int32.MaxValue version id rest'
                rest <- snd result
                ls <- fst result :: ls

            Operator (header, List.rev ls), rest

    let parseBitString (str: string) =
        let version, id, str' = parseHeader str

        if id = 4 then
            parseLiteral Int32.MaxValue (version, id) str'
        else
            parseOperator (version, id) str'

    let rec sumVersions =
        function
        | Literal ((version, _), _) -> version
        | Operator ((version, _), ls) -> version + Seq.sumBy sumVersions ls

type Year2021Day16() =
    interface ISolution with
        member _.year = 2021
        member _.day = 16

        member _.testA =
            seq [ (Int 6, Some "D2FE28")
                //   (Int 1, Some "38006F45291200")
                //   (Int 1, Some "EE00D40C823060")
                  (Int 16, Some "8A004A801A8002F478")
                  (Int 12, Some "620080001611562C8802118E34")
                  (Int 23, Some "C0015000016115A2E0802F182340")
                  (Int 31, Some "A0016C880162017C3686B18A3D4780")
                  ]

        member _.testB = seq []

        member _.solveA input =
            printfn "Solving %s" input
            parse input |> parseBitString |> fst |> sumVersions |> Int

        member _.solveB input = Int 0
