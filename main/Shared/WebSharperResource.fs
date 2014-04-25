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
type WebSharperResource private (name: string, args: string[], assemblyRequires: bool, deps: Set<string>) =

    static let ( ^. ) f x = f x

    static let pickler : Pickler.T<WebSharperResource> =
        Pickler.DefProduct (fun x y z deps -> WebSharperResource(x, Seq.toArray y, z, Set.ofSeq deps))
        ^. Pickler.Field (fun p -> p.Name) Pickler.String
        ^. Pickler.Field (fun p -> p.Args) (Pickler.Seq Pickler.String)
        ^. Pickler.Field (fun p -> p.IsAssemblyLevel) Pickler.Boolean
        ^. Pickler.Field (fun p -> p.Deps) (Pickler.Seq Pickler.String)
        ^. Pickler.EndProduct()

    /// Adds a resource requirement on this resource.
    member res.Require<'T>() =
        WebSharperResource(name, args, assemblyRequires, Set.add typeof<'T>.AssemblyQualifiedName deps)

    /// Arguments passed to the BaseResource constructor.
    member r.Args : seq<string> = Seq.ofArray args

    /// FQN of dependent resources.
    member internal r.Deps : seq<string> = Set.toSeq deps

    /// The class name of the resource.
    member r.Name = name

    /// Whether assembly requires the generated resoruce or not.
    member r.IsAssemblyLevel = assemblyRequires

    /// Creates a WebSharper resource descriptor that is required by the assembly by default.
    static member Create(name: string, [<ParamArray>] args: string[]) =
        if args.Length = 0 then
            invalidArg "args" "At least one argument is expected"
        WebSharperResource(name, args, true, Set.empty)

    /// Creates a WebSharper resource descriptor that is defined but not automatically required.
    static member CreateOptional(name: string, [<ParamArray>] args: string[]) =
        if args.Length = 0 then
            invalidArg "args" "At least one argument is expected"
        WebSharperResource(name, args, false, Set.empty)

    /// The pickler.
    static member internal Pickler = pickler
