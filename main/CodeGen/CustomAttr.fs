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

module A = IntelliFactory.WebSharper.Core.Attributes
module S = IntelliFactory.JavaScript.Syntax
module W = IntelliFactory.JavaScript.Writer
type P = IntelliFactory.JavaScript.Preferences

module CustomAttr =

    let GetReflectionOnlyAssembly (a: Assembly) =
        if a.ReflectionOnly then a else
            Assembly.ReflectionOnlyLoadFrom(a.Location)

    let T<'T> =
        let t = typeof<'T>
        let asm =
            t.Assembly
            |> GetReflectionOnlyAssembly
        asm.GetType(t.FullName)

    let Macro =
        let ctor = T<A.MacroAttribute>.GetConstructor([| typeof<Type> |])
        fun (t: Type) -> CustomAttributeBuilder(ctor, [| t |])

    let Call = Macro T<Macros.CallMacro>
    let New = Macro T<Macros.NewMacro>
    let Item = Macro T<Macros.ItemMacro>

    let ParamArrayCtor = T<ParamArrayAttribute>.GetConstructor([||])
    let ParamArray = CustomAttributeBuilder(ParamArrayCtor, [||])

    let InlineCtor =
        T<A.InlineAttribute>.GetConstructor([|typeof<string>|])

    let ExprToString e =
        W.ExpressionToString P.Compact e

    let Inline (e: S.Expression) =
        let s = ExprToString e
        CustomAttributeBuilder(InlineCtor, [| s |])

    let Str x =
        S.Constant (S.String x)

    let V n =
        S.Var ("$" + string n)

    let Method name numArgs =
        let args = List.init numArgs (fun i -> V (i + 1))
        Inline <| (V 0).[Str name].[args]

    let MethodWithParamArray (name: string) (numArgs: int) =
        let normalArgs = List.init (numArgs - 1) (fun i -> V (i + 1))
        let args = S.NewArray(List.map Some normalArgs).[Str "concat"].[[V numArgs]]
        Inline <| (V 0).[Str name].[Str "apply"].[[V 0; args]]

    let PropertyGet (name: string) =
        Inline <| (V 0).[Str name]

    let PropertySet (name: string) =
        Inline <| ((V 0).[Str name] ^= S.Var "$value").Void

    let rec Addr (name: NamePath) : S.Expression =
        match name with
        | NamePath.NP1 name ->
            (S.Var "$global").[Str name.Text]
        | NamePath.NP2 (parent, name) ->
            (Addr parent).[Str name.Text]

    let StaticPropertyGet (name: NamePath) =
        Inline (Addr name)

    let StaticPropertySet (name: NamePath) =
        Inline (Addr name ^= S.Var "$value").Void

    let StaticMethod name numArgs =
        let args = List.init numArgs V
        Inline <| (Addr name).[args]

    let StaticMethodWithParamArray name numArgs =
        let normalArgs = List.init (numArgs - 1) (fun i -> V i)
        let args = S.NewArray(List.map Some normalArgs).[Str "concat"].[[V (numArgs - 1)]]
        Inline <| (Addr name).[Str "apply"].[[!~ S.Null; args]]

    let Constructor name numArgs =
        let args = List.init numArgs V
        Inline <| S.New (Addr name, args)

    let RecordConstructor names =
        List.ofSeq names
        |> List.mapi (fun i n -> (n, V i))
        |> S.NewObject
        |> Inline
