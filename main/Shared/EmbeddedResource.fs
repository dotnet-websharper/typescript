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
type EmbeddedResource private (name: string, bytes: byte[], mime: option<string>) =

    static let ( ^. ) f x = f x

    static let pickler : Pickler.T<EmbeddedResource> =
        Pickler.DefProduct (fun x y z -> EmbeddedResource(x, y, z))
        ^. Pickler.Field (fun p -> p.Name) Pickler.String
        ^. Pickler.Field (fun p -> p.GetBytes()) Pickler.Bytes
        ^. Pickler.Field (fun p -> p.MimeType) (Pickler.Option Pickler.String)
        ^. Pickler.EndProduct()

    /// Opens the bytes as a stream.
    member internal r.OpenStream() =
        new MemoryStream(bytes, false) :> Stream

    /// Marks this as a WebResource, generating `System.Web.UI.WebResourceAttribute`.
    /// Requires a MIME type.
    member r.AsWebResource(mime: string) =
        EmbeddedResource(name, bytes, Some mime)

    /// Retrieves the raw bytes of the resource.
    member r.GetBytes() = Array.copy bytes

    /// Optional MIME type - for web resources marked with
    /// `System.Web.UI.WebResourceAttribute`.
    member r.MimeType = mime

    /// The name of the resource.
    member r.Name = name

    /// Creates an embedded resource.
    static member Create(name: string, bytes: byte[]) =
        EmbeddedResource(name, bytes, None)

    /// Creates an embedded resource from a given file.
    static member FromFile(path) =
        let name = Path.GetFileName(path)
        let content = File.ReadAllBytes(path)
        EmbeddedResource.Create(name, content)

    /// The pickler for resources.
    static member internal Pickler = pickler
