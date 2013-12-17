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

module Names =

    let ( == ) (a: obj) (b: obj) =
        Object.ReferenceEquals(a, b)

    let nullCheck (s: string) =
        match s with
        | null -> nullArg "Names cannot be null"
        | _ -> ()

    [<Sealed>]
    type Name(table: NameBuilder, name: string, stamp: int) =
        inherit Object()
        override x.Equals(o: obj) = base.Equals(o)
        override x.GetHashCode() = stamp
        override x.ToString() = name
        member x.Stamp = stamp
        member x.Table = table
        member x.Text = name

        interface IComparable with
            override x.CompareTo(o: obj) =
                match o with
                | _ when x == o -> 0
                | :? Name as o when table == x.Table -> compare stamp x.Stamp
                | _ -> invalidArg "o" "Invalid comparison"

    and [<Sealed>] NameBuilder() as self =
        let cache = ConcurrentDictionary<string,Name>()
        let mutable stamp = 0

        let makeId =
            Func<string,Name>(fun n ->
                let k = Interlocked.Increment(&stamp)
                Name(self, n, k))

        member t.CreateName(name) =
            nullCheck name
            cache.GetOrAdd(name, makeId)

        member t.ShareString(text) =
            match text with
            | null -> null
            | _ -> t.CreateName(text).Text

        static member Create() =
            NameBuilder()

    type NamePath<'N> =
        | NP1 of 'N
        | NP2 of NamePath<'N> * 'N

        member np.Name =
            match np with
            | NP1 n
            | NP2 (_, n) -> n

    type NamePath =
        NamePath<Name>

type Name = Names.Name
type NamePath = Names.NamePath<Name>
type NamePath<'T> = Names.NamePath<'T>
type NameTable<'T> = Dictionary<Name,'T>
