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
            match TryFind n ReflectionOnly with
            | None -> Assembly.ReflectionOnlyLoadFrom(a.Location)
            | Some r -> r

[<Sealed>]
type internal ReflectionUtility =

    static member GetReflectionOnlyAssembly(a: Assembly) =
        GetReflectionOnly a

    static member GetReflectionOnlyAssembly(t: Type) =
        ReflectionUtility.GetReflectionOnlyAssembly(t.Assembly)

    static member GetReflectionOnlyAssembly<'T>() =
        ReflectionUtility.GetReflectionOnlyAssembly(typeof<'T>.Assembly)

    static member GetReflectionOnlyType<'T>() =
        ReflectionUtility.GetReflectionOnlyType(typeof<'T>)

    static member GetReflectionOnlyType(t: Type) =
        let asm = ReflectionUtility.GetReflectionOnlyAssembly(t.Assembly)
        asm.GetType(t.FullName)
