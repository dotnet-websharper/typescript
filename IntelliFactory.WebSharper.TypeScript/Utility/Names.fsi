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

/// Implements identifiers and qualified names.
module internal Names =

    /// Represents a name with fast equality and comparison.
    [<Sealed>]
    type Name =
        interface IComparable

        /// The textual form of the name.
        member Text : string

    /// Represents a hierarhical name.
    type NamePath =
        | NP1 of Name
        | NP2 of NamePath * Name

        /// The outermost name.
        member Name : Name

    /// Builds names with hash-consing.
    [<Sealed>]
    type NameBuilder =

        /// Creates a new name.
        member CreateName : string -> Name

        /// Hash-conses a string.
        member ShareString : string -> string

        /// Creates a new name builder.
        static member Create : unit -> NameBuilder

type internal Name = Names.Name
type internal NamePath = Names.NamePath
type internal NameTable<'T> = Dictionary<Name,'T>
