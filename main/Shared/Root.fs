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

namespace IntelliFactory.WebSharper.TypeScript

/// Configures where generated code is placed:
/// under a given top-level class or namespace.
type Root =
    internal
    | ClassRoot of string
    | NamespaceRoot of string

    /// Indicates a top-level clas.
    static member Class(name) =
        ClassRoot name

    /// Indicates a top-level namespace.
    static member Namespace(name) =
        NamespaceRoot name

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Root =

    let inline private ( ^ ) f x =
        f x

    let Pickler =
        Pickler.DefSum (fun x k1 k2 ->
            match x with
            | ClassRoot f -> k1 f
            | NamespaceRoot b -> k2 b)
        ^ Pickler.Case ClassRoot Pickler.String
        ^ Pickler.LastCase NamespaceRoot Pickler.String
