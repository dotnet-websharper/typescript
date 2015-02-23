// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

namespace WebSharper.TypeScript

open System.Collections.Generic

module GraphColoring =

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

    type IConfig<'T> =
        abstract Edges : 'T -> seq<'T>
        abstract GetColor : 'T -> int
        abstract SetColor : 'T * int -> unit
        abstract Nodes : seq<'T>

    /// Picks the smallest possible color that is possible
    /// given current node edges.
    let PickColor (config: IConfig<'T>) (node: 'T) =
        config.Edges(node)
        |> Seq.map config.GetColor
        |> DistinctSorted
        |> PickSmallestUnique

    let ColorGraph (config: IConfig<'T>) =
        // Worst case: every node gets its index as color
        config.Nodes
        |> Seq.iteri (fun i node ->
            config.SetColor(node, i))
        // Improvement: for each node, pick smallest color
        // that is not one of the neighbor colors.
        for node in config.Nodes do
            let c = PickColor config node
            config.SetColor(node, c)
