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

module S = Shapes

module Contracts =

    type ContractKind<'T> = S.ContractKind<'T>
    type Indexer<'T> = S.Indexer<Name,'T>
    type Parameter<'T> = S.Parameter<Name,'T>
    type Signature<'T> = S.Signature<Name,'T>

    type Property<'T> =
        | OptProp of 'T
        | Prop of 'T

        member p.Value =
            match p with
            | OptProp v | Prop v -> v

    let indexer n t : Indexer<_> =
        {
            IndexerName = n
            IndexerType = t
        }

    let defaultHintPath =
        Names.NameBuilder.Create().CreateName("Anon")
        |> Names.NP1

    [<Sealed>]
    type Contract<'T>() =
        let mutable kind : ContractKind<'T> = S.EmptyContract
        let mutable byNumber : option<Indexer<'T>> = None
        let mutable byString : option<Indexer<'T>> = None
        let props = Dictionary<Name,Property<'T>>()
        let call = ResizeArray<Signature<'T>>()
        let ctors = ResizeArray<Signature<'T>>()
        let extends = ResizeArray<'T>()
        let mutable generics : list<Name> = []
        let mutable isUsedAsNamed = false

        member c.AddByNumber(n, t) =
            kind <- S.ObjectContract
            byNumber <- Some (indexer n t)

        member c.AddByString(n, t) =
            kind <- S.ObjectContract
            byString <- Some (indexer n t)

        member c.AddCall(s) =
            kind <-
                match kind, s with
                | S.EmptyContract, S.FunctionSignature (dom, r)->
                    S.FunctionContract (dom, r)
                | S.EmptyContract, _
                | S.FunctionContract _, _ -> S.MethodContract
                | _ -> S.ObjectContract
            call.Add(s)

        member c.AddNew(s) =
            kind <- S.ObjectContract
            ctors.Add(s)

        member c.AddOptProp(p, t) =
            kind <- S.ObjectContract
            props.Add(p, OptProp t)

        member c.AddProp(p, t) =
            kind <- S.ObjectContract
            props.Add(p, Prop t)

        member c.Extend(i) =
            extends.Add(i)

        member c.MarkNamedUse() =
            isUsedAsNamed <- true

        member c.SetGenerics(ns) =
            match ns with
            | [] -> ()
            | _ -> kind <- S.ObjectContract
            generics <- ns

        member c.ByNumber = byNumber
        member c.ByString = byString
        member c.Properties = props :> IReadOnlyDictionary<_,_>
        member c.Call = call :> seq<_>
        member c.Extends = extends :> seq<_>
        member c.IsUsedAsNamed = isUsedAsNamed
        member c.Kind = kind
        member c.Generics = generics
        member val HintPath = defaultHintPath with get, set
        member c.New = ctors :> seq<_>

    and [<Sealed>] Contracts<'T>() =
        let contracts = ResizeArray<Contract<'T>>()

        member x.Contract() =
            let c = Contract()
            contracts.Add(c)
            c

        member x.All =
            contracts :> seq<Contract<'T>>

    type Type =
        | TAny
        | TArray of Type
        | TBoolean
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

