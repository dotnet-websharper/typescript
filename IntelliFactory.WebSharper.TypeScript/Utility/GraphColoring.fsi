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

/// Solves a variant of graph coloring problem.
/// Given an undirected graph, assign an integer color
/// to every node, such that every edge connects nodes of
/// different color. Tries to minimize the number of colors
/// used - no hard guarantees, but trying to be better than
/// the naive solution of assigning a unique color to each node.
module internal GraphColoring =

    type IConfig<'T> =
        abstract Edges : 'T -> seq<'T>
        abstract GetColor : 'T -> int
        abstract SetColor : 'T * int -> unit
        abstract Nodes : seq<'T>

    val ColorGraph : IConfig<'T> -> unit

    /// INTERNAL: exposed for testing.
    val PickSmallestUnique : int [] -> int
