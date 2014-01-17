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

    let Types t =
        seq { for KeyValue (k, v) in t.T -> (k, v) }

    let InstallIntoModule (m: Sc.Module) key ty =
        m.ExportedContracts.Add(key, Sc.Foreign ty)

    let Install t (sc: Sc.Root) glob =
        for (name, ty) in Types t do
            match name with
            | NamePath.NP1 id ->
                InstallIntoModule glob id ty
            | NamePath.NP2 (mn, id) ->
                let m = sc.GetOrCreateModule(mn)
                InstallIntoModule m id ty

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

    let NamePickler : Pickler.T<NamePath> =
        Unchecked.defaultof<_>

    let TablePickler =
        Pickler.Dictionary NamePickler Pickler.Type
        |> Pickler.Wrap (fun x -> { T = x }) (fun x -> x.T)

    let Serialize t =
        Pickler.Pickle TablePickler t

    let Deserialize s =
        Pickler.ReadFromStream TablePickler s

    let TryParseAssembly (assem: Assembly) =
        match assem.GetManifestResourceStream(ResourceName) with
        | null -> None
        | stream ->
            use s = stream
            Some (Deserialize s)

    type Table with

        member t.Install(sc: Sc.Root, glob) =
            Install t sc glob

        member t.Serialize() =
            Array.empty<byte>

        static member Create(ts) =
            Create ts

        static member TryParseAssembly(a) =
            TryParseAssembly a

        static member Union(ts) =
            Union ts
