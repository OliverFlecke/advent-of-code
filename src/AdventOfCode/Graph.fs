module Graph

type Graph<'a when 'a: comparison> = Map<'a, 'a list>

let empty: Graph<'a> = Map.empty

let neighbors graph node =
    Map.tryFind node graph
    |> Option.defaultValue List.empty

module Undirected =
    let addEdge (graph: Graph<'a>) (f, t) =
        Map.add f (t :: neighbors graph f)
        <| Map.add t (f :: neighbors graph t) graph

module Directed =
    let directed (graph: Graph<'a>) (f, t) = Map.add f (t :: neighbors graph f)
