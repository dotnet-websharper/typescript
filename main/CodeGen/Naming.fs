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

module A = Analysis
module C = Contracts
module M = Memoization
module S = Shapes

module Naming =

    type Id = Ident.Id

    let Memo f =
        M.Memoize M.Options.Default f

    let MemoRec f =
        M.MemoizeRecursive M.Options.Default f

    type Indexer<'T> = Shapes.Indexer<Id,'T>
    type Parameter<'T> = Shapes.Parameter<Id,'T>
    type Signature<'T> = Shapes.Signature<Id,'T>

    type Property<'T> =
        {
            Id : Id
            Name : Name
            Optional : bool
            Type : 'T
        }

    [<ReferenceEquality>]
    [<NoComparison>]
    type Contract<'T> =
        {
            mutable ByNumber : option<Indexer<'T>>
            mutable ByString : option<Indexer<'T>>
            mutable Call : seq<Signature<'T>>
            mutable Extends : seq<'T>
            mutable Generics : seq<Id>
            IsAnonymous : bool
            mutable Name : Id
            mutable New : seq<Signature<'T>>
            mutable Origin : option<NamePath>
            mutable Properties : seq<Property<'T>>
        }

    let CreateContract isAnon name =
        {
            ByNumber = None
            ByString = None
            Call = Seq.empty
            Extends = Seq.empty
            Generics = Seq.empty
            IsAnonymous = isAnon
            Name = name
            New = Seq.empty
            Origin = None
            Properties = Seq.empty
        }

    type Type =
        | TAny
        | TArray of Type
        | TBoolean
        | TCompiled of System.Type * list<Type>
        | TGeneric of int
        | TGenericM of int
        | TNamed of Contract<Type> * list<Type>
        | TNumber
        | TString

    type Contract = Contract<Type>
    type Indexer = Indexer<Type>
    type Parameter = Parameter<Type>
    type Property = Property<Type>
    type Signature = Signature<Type>

    [<Sealed>]
    type ContractBuilder(idB: Ident.Builder) =
        let contractMap = Dictionary<C.Contract,Contract>()

        member p.Contract(c: C.Contract) : Contract =
            match contractMap.TryGetValue(c) with
            | true, r -> r
            | _ ->
                let map f xs = Seq.ofArray [| for x in xs -> f x |]
                let r : Contract = CreateContract c.IsAnonymous (idB.Id c.HintPath.Name.Text)
                contractMap.Add(c, r)
                r.ByNumber <- Option.map p.Indexer c.ByNumber
                r.ByString <- Option.map p.Indexer c.ByString
                r.Call <- map p.Signature c.Call
                r.Extends <- map p.Type c.Extends
                r.Generics <- [| for g in c.Generics -> idB.Id(g.Text) |]
                r.New <- map p.Signature c.New
                r.Origin <- Some c.HintPath
                r.Properties <- map p.Property c.Properties
                idB.LinkAll <| seq {
                    yield r.Name
                    yield! r.Generics
                    for p in r.Properties do
                        yield p.Id
                }
                r

        member p.Indexer(i: C.Indexer) : Indexer =
            {
                IndexerName = idB.Id(i.IndexerName.Text)
                IndexerType = p.Type(i.IndexerType)
            }

        member p.Parameter(par: C.Parameter) : Parameter =
            match par with
            | S.Param (name, t) -> Parameter.Param (idB.Id(name.Text), p.Type(t))
            | S.ParamConst (name, v) -> Parameter.ParamConst (idB.Id(name.Text), v)

        member p.Property(KeyValue (name, prop) : KeyValuePair<Name,C.Property>) : Property =
            {
                Id = idB.Id(name.Text)
                Name = name
                Optional = prop.IsOptional
                Type = p.Type(prop.Value)
            }

        member p.Signature(s: C.Signature) : Signature =
            {
                MethodGenerics = [for g in s.MethodGenerics -> idB.Id(g.Text)]
                Parameters = List.map p.Parameter s.Parameters
                RestParameter = Option.map p.Parameter s.RestParameter
                ReturnType = Option.map p.Type s.ReturnType
            }

        member p.Type(t: C.Type) =
            match t with
            | C.TAny -> TAny
            | C.TArray t -> TArray (p.Type(t))
            | C.TBoolean -> TBoolean
            | C.TCompiled (ty, ts) -> TCompiled (ty, List.map p.Type ts)
            | C.TGeneric k -> TGeneric k
            | C.TGenericM k -> TGenericM k
            | C.TLazy ty -> p.Type(ty.Value)
            | C.TNamed (con, ts) -> TNamed(p.Contract(con), List.map p.Type ts)
            | C.TNumber -> TNumber
            | C.TString -> TString

        static member Create(idSet, contracts) =
            let cb = ContractBuilder(idSet)
            for c in contracts do
                cb.Contract(c) |> ignore
            cb

    type Value =
        {
            Id : Id
            NamePath : NamePath
            Type : Type
        }

    [<Sealed>]
    type Module<'T>(id: 'T) =

        member val ContractList = ResizeArray<Contract>()
        member val ModuleList = ResizeArray<Module<Id>>()
        member val ValueList = ResizeArray<Value>()

        member m.Contracts = m.ContractList :> seq<_>
        member m.Id = id
        member m.Modules = m.ModuleList :> seq<_>
        member m.Values = m.ValueList :> seq<_>

    type NestedModule = Module<Id>
    type TopModule = Module<unit>

    let BuildModule (idB: Ident.Builder) (cB: ContractBuilder) (out: A.Output) =
        let topContainer = TopModule()
        let getContainer =
            MemoRec <| fun getContainer path ->
                match path : Names.NamePath with
                | Names.NP1 name ->
                    let m = Module(idB.Id(name.Text))
                    topContainer.ModuleList.Add(m)
                    m
                | Names.NP2 (p, name) ->
                    let p = getContainer p
                    let m = Module(idB.Id(name.Text))
                    p.ModuleList.Add(m)
                    m
        for c in out.Contracts do
            let contracts =
                match c.HintPath with
                | Names.NP1 _ -> topContainer.ContractList
                | Names.NP2 (path, _) -> getContainer.[path].ContractList
            contracts.Add(cB.Contract(c))
        for v in out.Values do
            let (values, hintName) =
                match v.HintPath with
                | Names.NP1 name -> (topContainer.ValueList, name)
                | Names.NP2 (path, name) -> (getContainer.[path].ValueList, name)
            values.Add {
                Id = idB.Id(hintName.Text)
                NamePath = v.NamePath
                Type = cB.Type(v.Type)
            }
        topContainer

    let rec LinkNames<'T> (idB: Ident.Builder) (path: list<Id>) (m: Module<'T>) : unit =
        idB.LinkAll <| seq {
            yield! path
            for m in m.Modules do
                yield m.Id
            for c in m.Contracts do
                yield c.Name
            for v in m.Values do
                yield v.Id
        }
        for sM in m.Modules do
            LinkNames idB (sM.Id :: path) sM

    let Do (out: A.Output) =
        let idB = Ident.Builder.Create()
        let cB = ContractBuilder.Create(idB, out.Contracts)
        let m = BuildModule idB cB out
        LinkNames idB [] m
        idB.Disambiguate()
        m

