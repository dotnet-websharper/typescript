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

[<AutoOpen>]
module internal Abbreviations =
    open System
    open System.Collections
    open System.Reflection
    open Microsoft.FSharp.Core

    type AppDomain = System.AppDomain
    type Assembly = Reflection.Assembly
    type AssemblyBuilder = Emit.AssemblyBuilder
    type AssemblyBuilderAccess = Emit.AssemblyBuilderAccess
    type AssemblyName = Reflection.AssemblyName
    type ConcurrentDictionary<'T1,'T2> = Concurrent.ConcurrentDictionary<'T1,'T2>
    type ConstructorInfo = Reflection.ConstructorInfo
    type EventArgs = System.EventArgs
    type EventHandler = System.EventHandler
    type Expr = Quotations.Expr
    type File = IO.File
    type Guid = System.Guid
    type Interlocked = Threading.Interlocked
    type IDisposable = System.IDisposable
    type IProvidedNamespace = CompilerServices.IProvidedNamespace
    type ITypeProvider = CompilerServices.ITypeProvider
    type MethodBase = Reflection.MethodBase
    type MethodInfo = Reflection.MethodInfo
    type Path = IO.Path
    type ParameterAttributes = Reflection.ParameterAttributes
    type ParameterInfo = Reflection.ParameterInfo
    type Type = System.Type
    type TypeAttributes = Reflection.TypeAttributes
    type TypeProviderAttribute = CompilerServices.TypeProviderAttribute
    type TypeProviderAssemblyAttribute = CompilerServices.TypeProviderAssemblyAttribute
    type TypeProviderConfig = CompilerServices.TypeProviderConfig
