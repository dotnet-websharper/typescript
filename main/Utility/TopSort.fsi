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

namespace WebSharper.TypeScript

/// Topological sorting.
module internal TopSort =

    /// Given a directed graph defined by a set of root nodes,
    /// a predecessor function and a notion of node identity,
    /// computes a topological sort of the nodes reachable from
    /// this set.
    val Explicit<'T> :
        rootNodes: seq<'T> ->
        getPredecessors: ('T -> seq<'T>) ->
        nodeIdentity: IEqualityComparer<'T> ->
        seq<'T>

    /// A variant of the algorithm using built-in equality.
    val Intrinsic<'T when 'T : equality> :
        rootNodes: seq<'T> ->
        getPredecessors: ('T -> seq<'T>) ->
        seq<'T>

