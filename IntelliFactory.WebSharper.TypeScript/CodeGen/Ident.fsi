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

module A = Analysis
module C = Contracts
module S = Shapes

/// Types and operations for creating CLR-safe, distinct identifiers.
module internal Ident =

    /// Represents a CLR-safe identifier.
    [<Sealed>]
    type Id =

        /// The string form of the identifier.
        member Text : string

    /// Constructs identifiers.
    [<Sealed>]
    type Builder =

        /// Disambiguates all created identifiers.
        member Disambiguate : unit -> unit

        /// Links multiple identifiers, making sure they are
        /// disambiguated to be distinct.
        member LinkAll : seq<Id> -> unit

        /// Constructs an identifier.
        member Id : string -> Id

        /// Identifier builder.
        static member Create : unit -> Builder
