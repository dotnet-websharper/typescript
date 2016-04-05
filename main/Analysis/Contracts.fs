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

namespace WebSharper.TypeScript

module S = Shapes

module Contracts =

    type Indexer<'T> = S.Indexer<Name,'T>
    type Parameter<'T> = S.Parameter<Name,'T>
    type Signature<'T> = S.Signature<Name,'T>

    type Property<'T> =
        | OptProp of 'T
        | Prop of 'T

        member p.IsOptional =
            match p with
            | OptProp _ -> true
            | _ -> false

        member p.Value =
            match p with
            | OptProp v | Prop v -> v

    let indexer n t : Indexer<_> =
        {
            IndexerName = n
            IndexerType = t
        }

    [<Sealed>]
    type Contract<'T>(isAnon, hintPath: NamePath) =
        let mutable byNumber : option<Indexer<'T>> = None
        let mutable byString : option<Indexer<'T>> = None
        let props = MultiDict<Name,Property<'T>>()
        let call = ResizeArray<Signature<'T>>()
        let ctors = ResizeArray<Signature<'T>>()
        let extends = ResizeArray<'T>()
        let mutable generics : list<Name> = []

        member val IsExtended = false with get, set
        member c.AddByNumber(n, t) = byNumber <- Some (indexer n t)
        member c.AddByString(n, t) = byString <- Some (indexer n t)
        member c.AddCall(s) = call.Add(s)
        member c.AddNew(s) = ctors.Add(s)
        member c.AddOptProp(p, t) = props.Add(p, OptProp t)
        member c.AddProp(p, t) = props.Add(p, Prop t)

        member c.Extend(i) = extends.Add(i)
        member c.SetGenerics(ns) = generics <- ns
        member c.ByNumber = byNumber
        member c.ByString = byString
        member c.Call = call :> seq<_>
        member c.Extends = extends :> seq<_>
        member c.Generics = generics
        member c.HintPath = hintPath //defaultHintPath with get, set
        member c.IsAnonymous = isAnon
        member c.New = ctors :> seq<_>
        member c.Props = props
        member c.Properties = props.All

    and [<Sealed>] Contracts<'T>() =
        let contracts = ResizeArray<Contract<'T>>()

        member x.AnonymousContract(hintPath) =
            let c = Contract(isAnon = true, hintPath = hintPath)
            contracts.Add(c)
            c

        member x.NamedContract(hintPath) =
            let c = Contract(isAnon = false, hintPath = hintPath)
            contracts.Add(c)
            c

        member x.All =
            contracts :> seq<Contract<'T>>

    type Type =
        | TAny
        | TArray of Type
        | TBoolean
        | TCompiled of System.Type * list<Type>
        | TGeneric of int
        | TGenericM of int
        | TLazy of Lazy<Type>
        | TNamed of Contract<Type> * list<Type>
        | TNumber
        | TString

    type Contract = Contract<Type>
    type Contracts = Contracts<Type>
    type Indexer = Indexer<Type>
    type Parameter = Parameter<Type>
    type Property = Property<Type>
    type Signature = Signature<Type>

    let TryGetAnonymousPropertyContract (c: Contract) name =
        c.Props.[name]
        |> List.tryPick (function
            | Prop (TNamed (c, [])) when c.IsAnonymous -> Some c
            | OptProp (TNamed (c, [])) when c.IsAnonymous -> Some c
            | _ -> None)
