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

module A = WebSharper.Core.Attributes
module J = WebSharper.Core.JavaScript.Core
module M = WebSharper.Core.Macros
module Q = WebSharper.Core.Quotations

/// Implements WebSharper-related macros for compilation support.
module Macros =

    let Call t e =
            match e with
            | Q.Call (_, target :: args) ->
                J.Application (t target, List.map t args)
            | _ ->
                failwith "Invalid application of the CallMacro"

    let Item t e =
            match e with
            | Q.PropertyGet (_, [target; arg]) ->
                (t target : J.Expression).[t arg]
            | Q.PropertySet (_, [target; arg; value]) ->
                J.FieldSet (t target, t arg, t value)
            | _ ->
                failwith "Invalid application of the ItemMacro"

    let New t e =
            match e with
            | Q.Call (_, target :: args) ->
                J.New (t target, List.map t args)
            | _ ->
                failwith "Invalid application of the NewMacro"

    [<Sealed>]
    type CallMacro() =
        interface M.IMacro with
            member this.Translate(e, t) = Call t e

    [<Sealed>]
    type ItemMacro() =
        interface M.IMacro with
            member this.Translate(e, t) = Item t e

    [<Sealed>]
    type NewMacro() =
        interface M.IMacro with
            member this.Translate(e, t) = New t e
