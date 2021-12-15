#r "nuget: FSharp.Data"
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"

open AdventOfCode.Core
open AdventOfCode.Types
open AdventOfCode.Utils

let data = input 2021 12
let testData = testInput 2021 12

let mediumTest =
    "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"

let largeTest =
    "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"

let parse =
    splitLines
    >> Seq.map (fun str -> str.Split('-'))
    >> Seq.map (fun xs -> xs.[0], xs.[1])
    >> Seq.fold Graph.Undirected.addEdge Graph.empty

let isUpper (str: string) = Seq.forall (System.Char.IsUpper) str
let isLower str = isUpper str |> not

let solve graph =
    let bfs start =
        let rec bfs' (paths: string list list) visited (frontier: string list list) =
            match frontier with
            | [] -> paths
            | path :: frontier' ->
                match path with
                | "end" :: _ as path -> bfs' (path :: paths) visited frontier'
                | node :: _ as path ->
                    let neighbors = Graph.neighbors graph node

                    neighbors
                    |> Seq.filter (flip Set.contains visited >> not)
                    |> Seq.map (fun n ->
                        let front = (n :: path) :: frontier'

                        let visited' =
                            if isUpper n then
                                visited
                            else
                                Set.add n visited

                        bfs' paths visited' front)
                    |> Seq.collect id
                    |> List.ofSeq

                | _ -> failwith $"Unable to handle path {path}"

        bfs' [] (set [ start ]) [ [ start ] ]

    bfs "start" |> List.length

let solver = parse >> solve

testSolution Level.One 10 <| solver testData
testSolution Level.One 19 <| solver mediumTest
testSolution Level.One 226 <| solver largeTest

submit 2021 12 Level.One <| solver data

// Part B
let solve' graph =
    let bfs start =
        let rec bfs' twice (paths: string list list) visited (frontier: string list list) =
            match frontier with
            | [] -> paths
            | path :: frontier' ->
                match path with
                | "end" :: _ as path -> bfs' twice (path :: paths) visited frontier'
                | node :: _ as path ->
                    let neighbors = Graph.neighbors graph node

                    neighbors
                    |> Seq.filter (flip Set.contains visited >> not)
                    |> Seq.map (fun n ->
                        let front = (n :: path) :: frontier'

                        if isUpper n || n = "start" || n = "end" then
                            seq { bfs' twice paths visited front }
                        else
                            seq {
                                if twice then
                                    yield bfs' twice paths (Set.add n visited) front
                                else
                                    yield bfs' true paths visited front
                                    yield bfs' false paths (Set.add n visited) front
                            })
                    |> Seq.collect id
                    |> Seq.collect id
                    |> List.ofSeq

                | _ -> failwith $"Unable to handle path {path}"

        bfs' false [] (set [ start ]) [ [ start ] ]


    let paths = bfs "start"

    // paths
    // |> Seq.map (Seq.rev >> String.concat ",")
    // |> Seq.sort
    // |> Seq.iter print

    paths |> Seq.distinct |> Seq.length

let solver' = parse >> solve'

testSolution Level.Two 36 <| solver' testData
testSolution Level.Two 103 <| solver' mediumTest
testSolution Level.Two 3509 <| solver' largeTest
submit 2021 12 Level.Two <| solver' data