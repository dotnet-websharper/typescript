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

/// Implements memoization tables.
module Memoization =

    type Options<'T1,'T2> =
        {
            Equality : IEqualityComparer<'T1>
            Laziness : option<Lazy<'T2> -> 'T2>
        }

    module Options =
        val Default<'T1,'T2> : Options<'T1,'T2>

    val Configure : unit -> Options<'T1,'T2>

    [<Sealed>]
    type Table<'T1,'T2> =
        member Clear : unit -> unit
        member Get : 'T1 -> 'T2
        member GetKeys : unit -> seq<'T1>
        member Remove : 'T1 -> bool
        member Count : int
        member Equality : IEqualityComparer<'T1>
        member Item : 'T1 -> 'T2 with get

    val Memoize<'T1,'T2> :
        Options<'T1,'T2> ->
        ('T1 -> 'T2) ->
        Table<'T1,'T2>

    val MemoizeRecursive<'T1,'T2> :
        Options<'T1,'T2> ->
        (('T1 -> 'T2) -> ('T1 -> 'T2)) ->
        Table<'T1,'T2>

    type Options<'T1,'T2> with
        member Memoize : ('T1 -> 'T2) -> Table<'T1,'T2>
        member MemoizeRecursive :
            (('T1 -> 'T2) -> ('T1 -> 'T2)) ->
            Table<'T1,'T2>
