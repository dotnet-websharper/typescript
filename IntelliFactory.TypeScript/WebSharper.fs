/// Implements WebSharper-related types and macros.
module IntelliFactory.TypeScript.WebSharper

open System
open IntelliFactory.WebSharper
module A = IntelliFactory.WebSharper.Core.Attributes
module J = IntelliFactory.JavaScript.Core
module M = IntelliFactory.WebSharper.Core.Macros
module Q = IntelliFactory.WebSharper.Core.Quotations

/// Represents TypeScript numbers.
[<Sealed>]
type Number =
    class
    end

/// Implements macros for compilation support.
module Macros =

    [<Sealed>]
    type CallMacro() =

        static let m : Core.Macros.Macro =
            {
                Body = None
                Expand = fun t e ->
                    match e with
                    | Q.Call (_, target :: args) ->
                        J.Application(t target, List.map t args)
                    | _ ->
                        failwith "Invalid application of the CallMacro"
                Requirements = []
            }

        interface M.IMacroDefinition with
            member this.Macro = m

    [<Sealed>]
    type ItemMacro() =

        static let m : Core.Macros.Macro =
            {
                Body = None
                Expand = fun t e ->
                    match e with
                    | Q.PropertyGet (_, [target; arg]) ->
                        (t target).[t arg]
                    | Q.PropertySet (_, [target; arg; value]) ->
                        J.FieldSet (t target, t arg, t value)
                    | _ ->
                        failwith "Invalid application of the ItemMacro"
                Requirements = []
            }

        interface M.IMacroDefinition with
            member this.Macro = m

    [<Sealed>]
    type NewMacro() =

        static let m : Core.Macros.Macro =
            {
                Body = None
                Expand = fun t e ->
                    match e with
                    | Q.Call (_, target :: args) ->
                        J.New (t target, List.map t args)
                    | _ ->
                        failwith "Invalid application of the NewMacro"
                Requirements = []
            }

        interface M.IMacroDefinition with
            member this.Macro = m
