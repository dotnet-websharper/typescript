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

/// See http://www.haskell.org/haskellwiki/Uniplate
module internal Uniplate =

    type R<'T1,'T2> = list<'T1> * (list<'T1> -> 'T2)
    type R<'T> = R<'T,'T>

    type Witness<'T1,'T2> =
        {
            Uniplate : 'T2 -> R<'T1,'T2>
        }

    type Witness<'T> = Witness<'T,'T>

    let Uniplate w x =
        w.Uniplate x

    let Children w x =
        fst (w.Uniplate x)

    let rec Universe w x =
        seq {
            yield x
            for c in Children w x do
                yield! Universe w c
        }

    let rec Transform w f x =
        let (cur, gen) = w.Uniplate x
        List.map (Transform w f) cur
        |> gen
        |> f

    let private BFlags =
        BindingFlags.NonPublic
        ||| BindingFlags.Public
        ||| BindingFlags.Instance
        ||| BindingFlags.Static

    [<Sealed>]
    exception RebuildError

    [<Sealed>]
    exception DerivationError of Type * Type

    /// Derives a witness by reflection for unions and records.
    let private Derive<'T1,'T2> () : Witness<'T1,'T2> =
        let t1 = typeof<'T1>
        let t2 = typeof<'T2>
        let inline restore build inc vs (children: list<_>) =
            let mutable children = children
            let vs = Array.copy vs
            for i in 0 .. vs.Length - 1 do
                if Array.get inc i then
                    let h = children.Head
                    children <- children.Tail
                    vs.[i] <- box h
            unbox (build vs)
        let makeInc (fs: seq<PropertyInfo>) =
            [|
                for field in fs ->
                    t1.IsAssignableFrom(field.PropertyType)
            |]
        let inline getChildren (vs: obj []) inc =
            let mutable children = []
            for i in 0 .. vs.Length - 1 do
                if Array.get inc i then
                    children <- (vs.[i] :?> 'T1) :: children
            List.rev children
        let inline makeUni (read: obj -> obj []) build inc x =
            let vs = read x
            (getChildren vs inc, restore build inc vs)
        if Reflection.FSharpType.IsUnion(t2, BFlags) then
            let cases = Reflection.FSharpType.GetUnionCases(t2, BFlags)
            let readTag = Reflection.FSharpValue.PreComputeUnionTagReader(t2, BFlags)
            let table =
                [|
                    for c in cases ->
                        let read = Reflection.FSharpValue.PreComputeUnionReader(c, BFlags)
                        let build = Reflection.FSharpValue.PreComputeUnionConstructor(c, BFlags)
                        let inc = makeInc (c.GetFields())
                        makeUni read build inc
                |]
            { Uniplate = fun inst -> table.[readTag inst] inst }
        elif Reflection.FSharpType.IsRecord(t2, BFlags) then
            let read = Reflection.FSharpValue.PreComputeRecordReader(t2, BFlags)
            let build = Reflection.FSharpValue.PreComputeRecordConstructor(t2, BFlags)
            let fields = Reflection.FSharpType.GetRecordFields(t2, BFlags)
            let inc = makeInc fields
            { Uniplate = makeUni read build inc }
        else
            raise (DerivationError (t1, t2))

    [<Sealed>]
    type private DeriveCache<'T1,'T2>() =
        static let inst = try Some (Derive<'T1,'T2>()) with DerivationError _ -> None
        static member Get() =
            match inst with
            | Some r -> r
            | None -> raise (DerivationError (typeof<'T1>, typeof<'T2>))

    [<Sealed>]
    type Witness =

        static member Auto<'T>() =
            DeriveCache<'T,'T>.Get()

        static member Auto<'T1,'T2>() =
            DeriveCache<'T1,'T2>.Get()

    [<Sealed>]
    type private SubType<'T1,'T2>() =

        static do
            let t1 = typeof<'T1>
            let t2 = typeof<'T2>
            let ok = t2.IsAssignableFrom(t1)
            if not ok then
                invalidArg (string t1) <| "Expecting a proper subtype of " + string t2

        static member Upcast(x: 'T1) : 'T2 =
            box x :?> 'T2

        static member TryDowncast(x: 'T2) : option<'T1> =
            match box x with
            | :? 'T1 as r -> Some r
            | _ -> None

    [<Sealed>]
    type NodeUtility<'T>(down: 'T -> R<'T>) =

        let w : Witness<'T> =
            { Uniplate = down }

        member u.Match(x: 'X) : R<'T> =
            let (xs, f) = Witness.Auto<'T,'X>().Uniplate(x)
            (xs, f >> SubType<'X,'T>.Upcast)

        member u.MatchList<'X>(xs: list<'X>, rebuild: list<'X> -> 'T) : R<'T> =
            let down x =
                match SubType<'X,'T>.TryDowncast(x) with
                | None -> raise RebuildError
                | Some r -> r
            (List.map SubType<'X,'T>.Upcast xs, List.map down >> rebuild)

        member u.Transform f x = Transform w f x
        member u.Universe x = Universe w x
        member u.Witness = w
