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

    let ValidIdentifierRegex =
        Regex(@"[_a-zA-Z][_a-zA-Z0-9]*")

    let InvalidCharacterRegex =
        Regex(@"[^_a-zA-Z0-9]")

    let KeywordSet =
        @"
            Item New Call

            abstract as base bool break byte case
            catch char checked class const continue decimal
            default delegate do double else enum event
            explicit extern false finally fixed float for
            foreach goto if implicit in int interface
            internal is lock long namespace new null
            object operator out override params private protected
            public readonly ref return sbyte sealed short
            sizeof stackalloc static string struct switch this
            throw true try typeof uint ulong unchecked
            unsafe ushort using virtual void volatile while

            asr land lor lsl lsr lxor mod sig

            abstract and as assert base begin class default delegate do
            done downcast downto elif else end exception extern false
            finally for fun function global if in inherit inline interface
            internal lazy let let! match member module mutable namespace
            new not null of open or override private public rec return return!
            select static struct then to true try type upcast use use!
            void when while with yield yield!

            atomic break checked component const constraint constructor
            continue eager event external fixed functor
            include method mixin object parallel process protected pure
            sealed tailcall trait virtual volatile
        "
        |> fun s ->
            HashSet(Regex.Split(s, @"\s+"))

    let IsKeyword k =
        KeywordSet.Contains(k)
        || k.StartsWith("get_")
        || k.StartsWith("set_")

    let TrailingNumbersRegex =
        Regex("\d+$")

    let Replace (p: Regex) (replace: string) (orig: string) =
        p.Replace(orig, replace)

    let MakeValidIdentifier name =
        let r =
            name
            |> Replace InvalidCharacterRegex "_"
            |> Replace TrailingNumbersRegex ""
        if ValidIdentifierRegex.IsMatch(r) && not (IsKeyword name)
            then name
            else "_" + name

    [<Sealed>]
    type Id private (stem: string) =
        let edges = HashSet<Id>()
        let mutable color = 0

        member n.Link(o: Id) =
            if n.Stem = stem then
                edges.Add(o) |> ignore
                o.Link(n)

        override n.ToString() =
            match color with
            | 0 -> stem
            | k -> stem + string color

        member val Color = 0 with get, set
        member n.Edges = edges :> seq<_>
        member n.Stem = stem
        member n.Text = n.ToString()

        static member Create(idSet: IdSet, name) =
            let r = Id(MakeValidIdentifier name)
            idSet.Ids.Add(r)
            r

        static member LinkAll(names: seq<Id>) =
            let names = Seq.toArray names
            for i in 0 .. names.Length - 1 do
                for j in i + 1 .. names.Length - 1 do
                    names.[i].Link(names.[j])
    and IdSet =
        {
            Ids : ResizeArray<Id>
        }

        member x.Create(name) =
            Id.Create(x, name)

        static member Empty() =
            { Ids = ResizeArray() }

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
            Type : 'T
        }

    [<ReferenceEquality>]
    [<NoComparison>]
    type Contract<'T> =
        {
            ByNumber : option<Indexer<'T>>
            ByString : option<Indexer<'T>>
            Call : seq<Signature<'T>>
            Extends : seq<'T>
            Generics : seq<Id>
            Kind : S.ContractKind<'T>
            Name : Id
            New : seq<Signature<'T>>
            Properties : seq<Property<'T>>
        }

    type Type =
        | TAny
        | TArray of Type
        | TBoolean
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
    type ContractBuilder(idSet: IdSet) =
        let contractMap = Dictionary<C.Contract,Contract>()

        member p.Contract(c: C.Contract) : Contract =
            match contractMap.TryGetValue(c) with
            | true, r -> r
            | _ ->
                let map f xs = Seq.ofArray [| for x in xs -> f x |]
                let r : Contract =
                    {
                        ByNumber = Option.map p.Indexer c.ByNumber
                        ByString = Option.map p.Indexer c.ByString
                        Call = map p.Signature c.Call
                        Extends = map p.Type c.Extends
                        Generics = [| for g in c.Generics -> idSet.Create(g.Text) |]
                        Kind = p.Kind(c.Kind)
                        Name = idSet.Create(c.HintPath.Name.Text)
                        New = map p.Signature c.New
                        Properties = map p.Property c.Properties
                    }
                contractMap.Add(c, r)
                Id.LinkAll <| seq {
                    yield r.Name
                    yield! r.Generics
                    for p in r.Properties do
                        yield p.Id
                }
                r

        member p.Indexer(i: C.Indexer) : Indexer =
            {
                IndexerName = idSet.Create(i.IndexerName.Text)
                IndexerType = p.Type(i.IndexerType)
            }

        member p.Kind(k: S.ContractKind<C.Type>) : S.ContractKind<Type> =
            match k with
            | S.EmptyContract -> S.EmptyContract
            | S.FunctionContract (xs, r) ->
                S.FunctionContract (List.map p.Type xs, Option.map p.Type r)
            | S.MethodContract -> S.MethodContract
            | S.ObjectContract -> S.ObjectContract

        member p.Parameter(par: C.Parameter) : Parameter =
            match par with
            | S.Param (name, t) -> Parameter.Param (idSet.Create(name.Text), p.Type(t))
            | S.ParamConst (name, v) -> Parameter.ParamConst (idSet.Create(name.Text), v)

        member p.Property(kv: KeyValuePair<Name,C.Property>) : Property =
            Unchecked.defaultof<_>

        member p.Signature(s: C.Signature) : Signature =
            {
                MethodGenerics = [for g in s.MethodGenerics -> idSet.Create(g.Text)]
                Parameters = List.map p.Parameter s.Parameters
                RestParameter = Option.map p.Parameter s.RestParameter
                ReturnType = Option.map p.Type s.ReturnType
            }

        member p.Type(t: C.Type) =
            match t with
            | C.TAny -> TAny
            | C.TArray t -> TArray (p.Type(t))
            | C.TBoolean -> TBoolean
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

    let BuildModule (idSet: IdSet) (cB: ContractBuilder) (out: A.Output) =
        let topContainer = TopModule()
        let getContainer =
            MemoRec <| fun getContainer path ->
                match path : Names.NamePath with
                | Names.NP1 name -> Module(idSet.Create(name.Text))
                | Names.NP2 (p, name) ->
                    let p = getContainer p
                    let m = Module(idSet.Create(name.Text))
                    p.ModuleList.Add(m)
                    m
        for c in out.Contracts do
            let contracts =
                match c.HintPath with
                | Names.NP1 _ -> topContainer.ContractList
                | Names.NP2 (path, _) -> getContainer.[path].ContractList
            contracts.Add(cB.Contract(c))
        for v in out.Values do
            let (values, name) =
                match v.NamePath with
                | Names.NP1 name -> (topContainer.ValueList, name)
                | Names.NP2 (path, name) -> (getContainer.[path].ValueList, name)
            values.Add {
                Id = idSet.Create(name.Text)
                NamePath = v.NamePath
                Type = cB.Type(v.Type)
            }
        topContainer

    let rec LinkNames<'T> (path: list<Id>) (m: Module<'T>) : unit =
        Id.LinkAll <| seq {
            yield! path
            for m in m.Modules do
                yield m.Id
            for c in m.Contracts do
                yield c.Name
            for v in m.Values do
                yield v.Id
        }
        for sM in m.Modules do
            LinkNames (sM.Id :: path) sM

    let GraphColoring (ids: IdSet) =
        GraphColoring.ColorGraph {
            new GraphColoring.IConfig<Id> with
                member cfg.Edges(id) = id.Edges
                member cfg.GetColor(id) = id.Color
                member cfg.SetColor(id, c) = id.Color <- c
                member cfg.Nodes = ids.Ids :> seq<_>
        }

    let Do (out: A.Output) =
        let idSet = IdSet.Empty()
        let cB = ContractBuilder.Create(idSet, out.Contracts)
        let m = BuildModule idSet cB out
        LinkNames [] m
        GraphColoring idSet
        m

    let (|MethodContract|_|) (c: Contract) =
        match c.Kind with
        | S.MethodContract -> Some c.Call
        | _ -> None

    let (|FunContract|_|) (c: Contract) =
        match c.Kind with
        | S.FunctionContract (dom, r) -> Some (dom, r)
        | _ -> None

    let (|MethodType|_|) ty =
        match ty with
        | TNamed (MethodContract res, []) -> Some res
        | _ -> None

    let (|FunType|_|) ty =
        match ty with
        | TNamed (FunContract (dom, r), []) -> Some (dom, r)
        | _ -> None

