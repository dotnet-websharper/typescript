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

/// Represents an WebSharper resource declaration.
[<Sealed>]
type WebSharperResource private (name: string, args: string[]) =

    static let ( ^. ) f x = f x

    static let pickler : Pickler.T<WebSharperResource> =
        Pickler.DefProduct (fun x y -> WebSharperResource.Create(x, Seq.toArray y))
        ^. Pickler.Field (fun p -> p.Name) Pickler.String
        ^. Pickler.Field (fun p -> p.Args) (Pickler.Seq Pickler.String)
        ^. Pickler.EndProduct()

    /// Arguments passed to the BaseResource constructor.
    member r.Args : seq<string> = Seq.ofArray args

    /// The class name of the resource.
    member r.Name = name

    /// Creates a WebSharper resource descriptor.
    static member Create(name: string, [<ParamArray>] args: string[]) =
        if args.Length = 0 then
            invalidArg "args" "At least one argument is expected"
        WebSharperResource(name, args)

    /// The pickler.
    static member internal Pickler = pickler
