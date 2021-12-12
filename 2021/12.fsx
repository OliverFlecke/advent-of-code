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

type Graph() =
    member val edges = Map.empty with get, set

    member this.addEdge(f, t) =
        this.edges <- Map.add f (t :: this.neighbors f) this.edges
        this.edges <- Map.add t (f :: this.neighbors t) this.edges

    member this.neighbors node =
        Map.tryFind node this.edges
        |> Option.defaultValue List.empty

let parse =
    splitLines
    >> Seq.map (fun str -> str.Split('-'))
    >> Seq.map (fun xs -> xs.[0], xs.[1])
    >> Seq.fold
        (fun (graph: Graph) edge ->
            graph.addEdge edge
            graph)
        (new Graph())

let isUpper (str: string) = Seq.forall (System.Char.IsUpper) str

let solve (graph: Graph) =
    let bfs start =
        let rec bfs' (paths: string list list) visited (frontier: string list list) =
            match frontier with
            | [] -> paths
            | path :: frontier' ->
                match path with
                | "end" :: _ as path -> bfs' (path :: paths) visited frontier'
                | node :: _ as path ->
                    let neighbors = graph.neighbors node

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

// testSolution Level.One 10 <| solver testData
// testSolution Level.One 19 <| solver mediumTest
// testSolution Level.One 226 <| solver largeTest

submit 2021 12 Level.One <| solver data
