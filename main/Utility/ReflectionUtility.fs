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

[<AutoOpen>]
module private ReflectionUtilityModule =

    type Kind =
        | ReflectionOnly
        | Regular

    let HasName (name: AssemblyName) (assem: Assembly) =
        assem.GetName().Name = name.Name

    let GetAssemblies kind =
        match kind with
        | ReflectionOnly -> AppDomain.CurrentDomain.ReflectionOnlyGetAssemblies()
        | Regular -> AppDomain.CurrentDomain.GetAssemblies()

    let TryFind name kind =
        GetAssemblies kind
        |> Array.tryFind (HasName name)

    let GetReflectionOnly (a: Assembly) =
        if a.ReflectionOnly then a else
            let n = a.GetName()
            match TryFind n Regular with
            | None -> Assembly.LoadFrom(a.Location)
            | Some r -> r

    let GetReflectionOnlyByName (n: string) =
        let ro =
            GetAssemblies Regular
            |> Array.tryFind (fun a -> a.GetName().Name = n)
        match ro with
        | Some r -> r
        | None ->
        let r =
            GetAssemblies Regular
            |> Array.find (fun a -> a.GetName().Name = n)
        Assembly.LoadFrom(r.Location) 


[<Sealed>]
type internal ReflectionUtility =

    static member GetReflectionOnlyAssembly(a: Assembly) =
        GetReflectionOnly a

    static member GetReflectionOnlyAssembly(n: string) =
        GetReflectionOnlyByName n

    static member GetReflectionOnlyAssembly(t: Type) =
        ReflectionUtility.GetReflectionOnlyAssembly(t.Assembly)

    static member GetReflectionOnlyAssembly<'T>() =
        ReflectionUtility.GetReflectionOnlyAssembly(typeof<'T>.Assembly)

    static member GetReflectionOnlyType<'T>() =
        ReflectionUtility.GetReflectionOnlyType(typeof<'T>)

    static member GetReflectionOnlyType(t: Type) =
        // for .NET Standard-compatible Reflection.Emit, reflection-only is not good
        t
