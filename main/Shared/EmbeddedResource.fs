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

/// Represents an embedded resource.
[<Sealed>]
type EmbeddedResource private (name: string, bytes: byte[]) =

    static let ( ^. ) f x = f x

    static let pickler : Pickler.T<EmbeddedResource> =
        Pickler.DefProduct (fun x y -> EmbeddedResource.Create(x, y))
        ^. Pickler.Field (fun p -> p.Name) Pickler.String
        ^. Pickler.Field (fun p -> p.GetBytes()) Pickler.Bytes
        ^. Pickler.EndProduct()

    /// Opens the bytes as a stream.
    member internal r.OpenStream() =
        new MemoryStream(bytes, false) :> Stream

    /// Retrieves the raw bytes of the resource.
    member r.GetBytes() = Array.copy bytes

    /// The name of the resource.
    member r.Name = name

    /// Creates an embedded resource.
    static member Create(name: string, bytes: byte[]) =
        EmbeddedResource(name, bytes)

    /// Creates an embedded resource from a given file.
    static member FromFile(path) =
        let name = Path.GetFileName(path)
        let content = File.ReadAllBytes(path)
        EmbeddedResource.Create(name, content)

    /// The pickler for resources.
    static member internal Pickler = pickler
