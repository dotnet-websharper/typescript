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

module Contracts =

    type Parameter<'T> =
        | Param of Name * 'T
        | ParamConst of Name * string

    let isSimpleParameter p =
        match p with
        | Param _ -> true
        | _ -> false

    let getParameterType p =
        match p with
        | Param (_, t) -> Some t
        | _ -> None

    type Signature<'T> =
        {
            MethodGenerics : list<Name>
            Parameters : list<Parameter<'T>>
            RestParameter : option<Parameter<'T>>
            ReturnType : option<'T>
        }

    let isFunctionSignature si =
        si.RestParameter.IsNone
        && si.MethodGenerics.IsEmpty
        && List.forall isSimpleParameter si.Parameters

    let (|FunctionSignature|_|) si =
        if isFunctionSignature si then
            Some (List.choose getParameterType si.Parameters, si.ReturnType)
        else None

    type Property<'T> =
        | OptProp of 'T
        | Prop of 'T

        member p.Value =
            match p with
            | OptProp v | Prop v -> v

    type Indexer<'T> =
        {
            IndexerName : Name
            IndexerType : 'T
        }

    let indexer n t =
        {
            IndexerName = n
            IndexerType = t
        }

    let defaultHintPath =
        Names.NameBuilder.Create().CreateName("Anon")
        |> Names.NP1

    type ContractKind<'T> =
        | EmptyContract
        | FunctionContract of list<'T> * option<'T>
        | MethodContract
        | ObjectContract

    [<Sealed>]
    type Contract<'T>() =
        let mutable kind : ContractKind<'T> = EmptyContract
        let mutable byNumber : option<Indexer<'T>> = None
        let mutable byString : option<Indexer<'T>> = None
        let props = Dictionary<Name,Property<'T>>()
        let call = ResizeArray<Signature<'T>>()
        let ctors = ResizeArray<Signature<'T>>()
        let extends = ResizeArray<'T>()
        let mutable generics : list<Name> = []

        member c.AddByNumber(n, t) =
            kind <- ObjectContract
            byNumber <- Some (indexer n t)

        member c.AddByString(n, t) =
            kind <- ObjectContract
            byString <- Some (indexer n t)

        member c.AddCall(s) =
            kind <-
                match kind, s with
                | EmptyContract, FunctionSignature (dom, r)->
                    FunctionContract (dom, r)
                | EmptyContract, _
                | FunctionContract _, _ -> MethodContract
                | _ -> ObjectContract
            call.Add(s)

        member c.AddNew(s) =
            kind <- ObjectContract
            ctors.Add(s)

        member c.AddOptProp(p, t) =
            kind <- ObjectContract
            props.Add(p, OptProp t)

        member c.AddProp(p, t) =
            kind <- ObjectContract
            props.Add(p, Prop t)

        member c.Extend(i) =
            extends.Add(i)

        member c.SetGenerics(ns) =
            match ns with
            | [] -> ()
            | _ -> kind <- ObjectContract
            generics <- ns

        member c.ByNumber = byNumber
        member c.ByString = byString
        member c.Properties = props :> IReadOnlyDictionary<_,_>
        member c.Call = call :> seq<_>
        member c.Extends = extends :> seq<_>
        member c.Kind = kind
        member c.Generics = generics
        member val HintPath = defaultHintPath with get, set
        member c.New = ctors :> seq<_>

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
    type Indexer = Indexer<Type>
    type Parameter = Parameter<Type>
    type Property = Property<Type>
    type Signature = Signature<Type>

    let (|MethodContract|_|) (c: Contract) =
        match c.Kind with
        | MethodContract -> Some c.Call
        | _ -> None

    let (|FunContract|_|) (c: Contract) =
        match c.Kind with
        | FunctionContract (dom, r) -> Some (dom, r)
        | _ -> None

    let (|MethodType|) ty =
        match ty with
        | TNamed (MethodContract res, []) -> Some res
        | _ -> None

    let (|FunType|_|) ty =
        match ty with
        | TNamed (FunContract (dom, r), []) -> Some (dom, r)
        | _ -> None

