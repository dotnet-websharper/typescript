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
/// different color. Try to minimize the number of colors
/// used - no hard guarantees, but trying to be better than
/// the naive solution of assigning a unique color to each node.
module internal GraphColoring =

    /// Represents a coloring of the graph.
    [<Sealed>]
    type Coloring<'T>

    /// Represents a graph.
    [<Sealed>]
    type Graph<'T>

    /// Represents a node. Node identity is modelled with
    /// reference equality.
    [<Sealed>]
    type Node<'T>

    /// Retrieves the color of the node.
    val Color : Coloring<'T> -> Node<'T> -> int

    /// Computes a coloring for the given graph.
    val ComputeColoring : Graph<'T> -> Coloring<'T>

    /// Defines a graph.
    val CreateGraph : seq<Node<'T>> -> seq<Node<'T> * Node<'T>> -> Graph<'T>

    /// Creates a new node with a given label.
    val CreateNode : 'T -> Node<'T>

    /// Fetches the node label.
    val Label : Node<'T> -> 'T

    /// INTERNAL: exposed for testing.
    val PickSmallestUnique : int [] -> int
