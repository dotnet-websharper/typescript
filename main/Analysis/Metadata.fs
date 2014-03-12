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

module C = Contracts
module S = Syntax
module Sc = Scopes

module Metadata =

    let ResourceName =
        "WebSharper.TypeScript.bin"

    [<NoComparison>]
    [<ReferenceEquality>]
    type Table =
        {
            T : Dictionary<NamePath,Type>
        }

    let ToReflectionOnly x =
        let r = Dictionary()
        for KeyValue (k, v) in x.T do
            r.[k] <- ReflectionUtility.GetReflectionOnlyType(v)
        { T = r }

    let Types t =
        seq { for KeyValue (k, v) in t.T -> (k, v) }

    let InstallIntoScope (sc: Sc.Scope) key ty =
        sc.BindContract(key, Sc.Foreign ty)

    let Install t (sc: Sc.Root) (glob: Sc.Scope) =
        for (name, ty) in Types t do
            match name with
            | NamePath.NP1 id ->
                InstallIntoScope glob id ty
            | NamePath.NP2 (mn, id) ->
                let m = sc.GetOrCreateScope(mn)
                InstallIntoScope m id ty

    let Union ts =
        let d = Dictionary()
        for t in ts do
            for kv in t.T do
                d.[kv.Key] <- kv.Value // TODO: detect & report conflicts
        { T = d }

    let Create ts =
        let d = Dictionary()
        for (k, v) in ts do
            d.[k] <- v // TODO: detect & report conflicts
        { T = d }

    let inline ( ^ ) f x =
        f x

    type N = Name

    let NamePickler (builder: Names.NameBuilder) : Pickler.T<Name> =
        Pickler.String
        |> Pickler.Wrap
            (fun str -> builder.CreateName(str))
            (fun name -> name.Text)

    let NamePathPickler builder : Pickler.T<NamePath> =
        let name = NamePickler builder
        Pickler.Fix <| fun self ->
            Pickler.DefSum (fun x k1 k2 ->
                match x with
                | NamePath.NP1 v -> k1 v
                | NamePath.NP2 (x, y) -> k2 (x, y))
            ^ Pickler.Case NamePath.NP1 name
            ^ Pickler.LastCase NamePath.NP2 (Pickler.Pair self name)

    let TablePickler builder =
        Pickler.Dictionary (NamePathPickler builder) Pickler.Type
        |> Pickler.Wrap (fun x -> { T = x }) (fun x -> x.T)

    let Serialize builder t =
        Pickler.Pickle (TablePickler builder) t

    let Deserialize builder s =
        Pickler.ReadFromStream (TablePickler builder) s

    let TryParseAssembly builder (assem: Assembly) =
        try
            use s = assem.GetManifestResourceStream(ResourceName)
            match s with
            | null -> None
            | s -> Some (Deserialize builder s)
        with e ->
            failwithf "Failed to parse assembly: %O with error %O" assem e

    type Table with

        member t.Install(sc: Sc.Root, glob) =
            Install t sc glob

        member t.Serialize() =
            let builder = Names.NameBuilder.Create() // does not matter, only for de-serialization.
            Pickler.Pickle (TablePickler builder) t

        static member Create(ts) =
            Create ts
            |> ToReflectionOnly

        static member TryParseAssembly(builder, a) =
            TryParseAssembly builder a
            |> Option.map ToReflectionOnly

        static member Union(ts) =
            Union ts
