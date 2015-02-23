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

/// Represents an assembly referenced during the TypeScript compilation process.
type ReferenceAssembly =
    internal
    | ReferenceAssemblyBytes of byte []
    | ReferenceAssemblyFile of string

    /// References a given file by its path.
    static member File(path) =
        ReferenceAssemblyFile path

    /// References a raw assembly from an array of bytes.
    static member Raw(bytes) =
        ReferenceAssemblyBytes bytes

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal ReferenceAssembly =

    let inline private ( ^ ) f x =
        f x

    let Pickler =
        Pickler.DefSum (fun x k1 k2 ->
            match x with
            | ReferenceAssemblyFile f -> k1 f
            | ReferenceAssemblyBytes b -> k2 b)
        ^ Pickler.Case ReferenceAssemblyFile Pickler.String
        ^ Pickler.LastCase ReferenceAssemblyBytes Pickler.Bytes
