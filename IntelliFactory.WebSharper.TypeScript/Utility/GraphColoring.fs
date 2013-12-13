// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

namespace IntelliFactory.WebSharper.TypeScript

module GraphColoring =

    [<Sealed>]
    type Node<'T>(v: 'T) =
        member val Color = 0 with get, set
        member val Edges = ResizeArray<Node<'T>>()
        member n.Label =  v

    [<Sealed>]
    type Graph<'T>(nodes: seq<Node<'T>>, edges: seq<Node<'T> * Node<'T>>) =

        // Cached edges with duplicates removed.
        let edges = Seq.toArray (Seq.distinct edges)

        // All nodes (from nodes and edges) with duplicates removed.
        let nodes =
            seq {
                yield! nodes
                for (a, b) in edges do
                    yield a
                    yield b
            }
            |> Seq.distinct
            |> Seq.toArray

        member g.Edges = edges
        member g.Nodes = nodes

    [<Sealed>]
    type Coloring<'T>(map: Dictionary<Node<'T>, int>) =
        member c.Item with get x = map.[x]

    let Label (n: Node<'T>) =
        n.Label

    let Color (c: Coloring<'T>) node =
        c.[node]

    let CreateNode value =
        Node(value)

    let CreateGraph nodes edges =
        Graph(nodes, edges)

    /// Links one node to another.
    let LinkTo (a: Node<'T>) (b: Node<'T>) =
        if a.Edges.Contains(b) |> not then
            a.Edges.Add(b)

    /// Links two nodes in both directions.
    let Link a b =
        LinkTo a b
        LinkTo b a

    /// Clears all state in the graph, resets edges.
    let Reset (graph: Graph<'T>) =
        for node in graph.Nodes do
            node.Color <- 0
            node.Edges.Clear()
        for (a, b) in graph.Edges do
            Link a b

    /// Finds the smallest non-negative int
    /// that is not contained in the sorted input array.
    let PickSmallestUnique input =
        let len = Array.length input
        let rec loop cand pos =
            if pos >= len then cand else
                let c = input.[pos]
                match compare c cand with
                | 0 -> loop (c + 1) (pos + 1)
                | 1 -> cand
                | _ -> loop cand (pos + 1)
        loop 0 0

    /// Sorts and removes duplicates.
    let DistinctSorted (input: seq<int>) : int [] =
        let set = SortedSet(input)
        let r = Array.zeroCreate set.Count
        set.CopyTo(r)
        r

    /// Picks the smallest possible color that is possible
    /// given current node edges.
    let PickColor (node: Node<'T>) =
        seq { for n in node.Edges -> n.Color }
        |> DistinctSorted
        |> PickSmallestUnique

    let ComputeColoring g =
        let d = Dictionary()
        // Clear the state from previous runs if any
        Reset g
        // Worst case: every node gets its index as color
        g.Nodes |> Seq.iteri (fun i node -> node.Color <- i)
        // Improvement: for each node, pick smallest color
        // that is not one of the neighbor colors.
        for node in g.Nodes do
            let c = PickColor node
            node.Color <- c
            d.[node] <- c
        Coloring(d)
