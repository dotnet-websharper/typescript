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

[<AbstractClass>]
type DelegatingProvider(p: ITypeProvider) =

    interface IDisposable with

        member this.Dispose() =
            p.Dispose()

    interface ITypeProvider with

        member this.ApplyStaticArguments(t, className, args) =
            p.ApplyStaticArguments(t, className, args)

        member this.GetGeneratedAssemblyContents(assem) =
            p.GetGeneratedAssemblyContents(assem)

        member this.GetInvokerExpression(mb, par) =
            p.GetInvokerExpression(mb, par)

        member this.GetNamespaces() =
            p.GetNamespaces()

        member this.GetStaticParameters(t) =
            p.GetStaticParameters(t)

        [<CLIEvent>]
        member this.Invalidate =
            p.Invalidate
