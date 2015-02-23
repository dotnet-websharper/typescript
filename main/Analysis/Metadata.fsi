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

/// Support for metadata tables correlating TypeScript contracts to
/// already compiled .NET types. This is important for separate compilation.
module internal Metadata =

    /// The constant used as a resource name to write metadata to assemblies.
    val ResourceName : string

    /// Represents a mapping from TypeScript type names to CLR types.
    [<Sealed>]
    type Table =

        /// Adds bindings from the table to the global root and scope.
        member Install : globalScope: Scopes.Root * globalScope: Scopes.Scope -> unit

        /// Serializes to a byte array.
        member Serialize : unit -> byte []

        /// Creates a new table using the given pairings.
        static member Create : seq<NamePath * Type> -> Table

        /// Tries to parse a table from an assembly.
        static member TryParseAssembly : Names.NameBuilder * Assembly -> option<Table>

        /// Takes a union of multiple tables.
        static member Union : seq<Table> -> Table
