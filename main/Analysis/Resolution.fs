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

module M = Model
module S = Syntax
module Sh = Shapes

module internal Resolution =

    type ContractFlags =
        | IsAnonymous = 1
        | None = 0

    [<NoComparison>]
    [<ReferenceEquality>]
    type Contract =
        {
            mutable CByNumber : option<Indexer>
            mutable CByString : option<Indexer>
            mutable CCall : list<Signature>
            mutable CExtends : list<Type>
            mutable CFlags : ContractFlags
            mutable CGenerics : list<Name>
            mutable CHintPath : NamePath
            mutable CKey : M.Contract
            mutable CNew : list<Signature>
            mutable CProperties : list<Property>
        }

    and Indexer = Sh.Indexer<Name,Type>
    and Parameter = Sh.Parameter<Name,Type>

    and [<NoComparison>] [<ReferenceEquality>] Property =
        {
            PName : Name
            POptional : bool
            PType : Type
        }

    and Signature = Sh.Signature<Name,Type>

    and Type =
        | TAny
        | TArray of Type
        | TBoolean
        | TCompiled of System.Type * list<Type>
        | TContract of Contract * list<Type>
        | TGeneric of int
        | TGenericM of int
        | TNumber
        | TString

    and [<NoComparison>] [<ReferenceEquality>] Value =
        {
            VHintPath : NamePath
            VNamePath : NamePath
            VType : Type
        }

    type Input =
        {
            Facts : seq<M.Fact>
            Logger : Logger
        }

    type Output =
        {
            Contracts : seq<Contract>
            Values : seq<Value>
        }

    type Contract with
        member c.ByNumber = c.CByNumber
        member c.ByString = c.CByString
        member c.Call = Seq.ofList c.CCall
        member c.Extends = Seq.ofList c.CExtends
        member c.Generics = c.CGenerics
        member c.HintPath = c.CHintPath
        member c.IsAnonymous = c.CFlags.HasFlag(ContractFlags.IsAnonymous)
        member c.New = Seq.ofList c.CNew
        member c.Properties = Seq.ofList c.CProperties

        static member Create(key) =
            {
                CByNumber = None
                CByString = None
                CCall = []
                CExtends = []
                CFlags = enum 0
                CGenerics = []
                CHintPath = Unchecked.defaultof<_>
                CKey = key
                CNew = []
                CProperties = []
            }

        member c.Set(other) =
            c.CByNumber <- other.CByNumber
            c.CByString <- other.CByString
            c.CCall <- other.CCall
            c.CExtends <- other.CExtends
            c.CFlags <- other.CFlags
            c.CGenerics <- other.CGenerics
            c.CHintPath <- other.CHintPath
            c.CKey <- other.CKey
            c.CNew <- other.CNew
            c.CProperties <- other.CProperties

    type Property with
        member p.IsOptional = p.POptional
        member p.Name = p.PName
        member p.Type = p.PType

    type Value with
        member v.HintPath = v.VHintPath
        member v.NamePath = v.VNamePath
        member v.Type = v.VType

    type Type with
        member t.WithTypeGenerics(gs) =
            match gs with
            | [] -> t
            | _ ->
                let inline ( ! ) (x: Type) = x.WithTypeGenerics(gs)
                match t with
                | TArray x -> TArray !x
                | TCompiled (m, ts) -> TCompiled (m, List.map (!) ts)
                | TContract (c, ts) -> TContract (c, List.map (!) ts)
                | TGeneric n -> List.nth gs n
                | _ -> t

    type Key =
        | BoundContractKey of M.Scope * Name
        | BoundModuleKey of M.Scope * Name
        | BoundVarKey of M.Scope * Name
        | ContractFact of M.Contract

        static member KeysFromFact(fact) =
            match fact with
            | M.BoundContract (s, n, _) -> [BoundContractKey (s, n)]
            | M.BoundImport (s, n, _, _, _) -> [BoundContractKey (s, n); BoundModuleKey (s, n); BoundVarKey (s, n)]
            | M.BoundModule (s, n, _) -> [BoundModuleKey (s, n)]
            | M.BoundModuleImport (s, n, _, _) -> [BoundModuleKey (s, n)]
            | M.BoundVar (s, n, _) -> [BoundVarKey (s, n)]
            | M.CanCall (c, _)
            | M.CanConstruct (c, _)
            | M.CanIndexByNumber (c, _, _)
            | M.CanIndexByString (c, _, _)
            | M.HasProperty (c, _, _, _) -> [ContractFact c]
            | _ -> []

    [<Sealed>]
    type MultiDict<'K,'V when 'K : equality> private (d: Dictionary<'K,list<'V>>) =

        static let get (d: Dictionary<'K,list<'V>>) (key: 'K) =
            let mutable r = []
            d.TryGetValue(key, &r) |> ignore
            r

        member md.Get(key) =
            get d key

        static member Create(vs: seq<'V>, t: 'V -> list<'K>) =
            let d = Dictionary()
            for v in vs do
                let ks = t v
                for k in ks do
                    d.[k] <- v :: get d k
            MultiDict<'K,'V>(d)

    [<Sealed>]
    type Resolver private
        (
            db: MultiDict<Key,M.Fact>,
            input: Input
        ) =

        let contracts = Dictionary<M.Contract,Contract>()
        let typeQuerySet = HashSet<M.Scope * S.TypeQuery>()
        let importSet = HashSet<obj>(HashIdentity.Reference)

        member r.BuildContract(id, info: M.ContractInfo) =
            let facts = r.GetFactsByKey(Key.ContractFact id)
            {
                CByNumber =
                    facts
                    |> List.tryPick (function
                        | M.CanIndexByNumber (_, name, ty) ->
                            Some {
                                IndexerName = name
                                IndexerType = r.Type(ty)
                            }
                        | _ -> None)
                CByString =
                    facts
                    |> List.tryPick (function
                        | M.CanIndexByString (_, name, ty) ->
                            Some {
                                IndexerName = name
                                IndexerType = r.Type(ty)
                            }
                        | _ -> None)
                CCall =
                    facts
                    |> List.choose (function
                        | M.CanCall (_, s) -> Some (r.Signature(s))
                        | _ -> None)
                CExtends = List.map r.Type info.Extends
                CFlags =
                    let mutable flags = ContractFlags.None
                    if info.Flags.HasFlag(M.ContractFlags.IsAnonymous) then
                        flags <- flags ||| ContractFlags.IsAnonymous
                    flags
                CGenerics = info.Generics
                CHintPath = info.HintPath
                CKey = id
                CNew =
                    facts
                    |> List.choose (function
                        | M.CanConstruct (_, s) -> Some (r.Signature(s))
                        | _ -> None)
                CProperties =
                    facts
                    |> List.choose (function
                        | M.HasProperty (_, name, ty, opt) -> Some (name, ty, opt)
                        | _ -> None)
                    |> Seq.distinctBy (fun (name, _, _) -> name)
                    |> Seq.toList
                    |> List.map (fun (name, ty, opt) ->
                        {
                            PName = name
                            POptional = opt
                            PType = r.Type(ty)
                        })
            }

        member r.Contract(id) =
            let mutable res = Unchecked.defaultof<_>
            if contracts.TryGetValue(id, &res) |> not then
                res <- Contract.Create(id)
            res

        member r.GetContracts() =
            r.GetFacts()
            |> Seq.choose (function
                | M.IsContract (cId, cInfo) ->
                    let c = r.Contract(cId)
                    c.Set(r.BuildContract(cId, cInfo))
                    Some c
                | _ -> None)
            |> Seq.toArray
            |> Seq.ofArray

        member r.GetFacts() =
            input.Facts

        member r.GetFactsByKey(key) : list<M.Fact> =
            db.Get(key)

        member r.GetValues() =
            r.GetFacts()
            |> Seq.choose (function
                | M.IsValue v ->
                    Some {
                        VHintPath = v.HintPath
                        VNamePath = v.NamePath
                        VType = r.Type(v.Type)
                    }
                | _ -> None)
            |> Seq.toArray
            |> Seq.ofArray

        member r.IsContract(c) =
            contracts.ContainsKey(c)

        member r.ResolveModuleId(scope, id) =
            let facts = r.GetFactsByKey(BoundModuleKey (scope, id))
            let res =
                let f =
                    facts
                    |> List.choose (function
                        | M.BoundModule (_, _, m) -> Some m
                        | _ -> None)
                match f with
                | [] -> None
                | [m] -> Some m
                | m :: _ -> Some m // TODO: warning
            match res with
            | None ->
                // NOTE: using importSet to break potentially infinite recursion
                // during module resolution via import declarations.
                match facts with
                | M.BoundImport (_, _, chain, mN, n) as fact :: _ -> // TODO: warning on tails.
                    if importSet.Add(fact) then
                        r.ResolveModuleNameInChain(chain, S.MN2 (mN, n))
                    else None
                | M.BoundModuleImport (_, _, chain, name) as fact :: _ -> // TODO: warning on tails.
                    if importSet.Add(fact) then
                        r.ResolveModuleIdInChain(chain, name)
                    else None
                | _ -> None
            | res -> res

        member r.ResolveModuleIdInChain(chain, id) =
            match chain with
            | M.InitialScope sc -> r.ResolveModuleId(sc, id)
            | M.SubScope (parent, sc) ->
                match r.ResolveModuleId(sc, id) with
                | None -> r.ResolveModuleIdInChain(parent, id)
                | r -> r

        member r.ResolveModuleName(scope, name) =
            match name with
            | S.MN1 id -> r.ResolveModuleId(scope, id)
            | S.MN2 (mN, id) ->
                match r.ResolveModuleName(scope, mN) with
                | None -> None
                | Some m -> Some (M.SubModule (m, id))

        member r.ResolveModuleNameInChain(chain, name) =
            match chain with
            | M.InitialScope sc -> r.ResolveModuleName(sc, name)
            | M.SubScope (parent, sc) ->
                match r.ResolveModuleName(sc, name) with
                | None -> r.ResolveModuleNameInChain(parent,  name)
                | r -> r

        member r.ResolvePropertyType(c: M.Contract, name: Name, generics: list<Type>) =
            ContractFact c
            |> r.GetFactsByKey
            |> List.tryPick (function
                | M.HasProperty (_, n, t, _) when name = n ->
                    r.Type(t).WithTypeGenerics(generics)
                    |> Some
                | _ -> None)

        member r.ResolveTypeId(scope, id) =
            let facts = r.GetFactsByKey(BoundContractKey (scope, id))
            let res =
                let f =
                    facts
                    |> List.choose (function
                        | M.BoundContract (_, _, c) -> Some c
                        | _ -> None)
                match f with
                | [] -> None
                | [c] -> Some c
                | c :: _ -> Some c // TODO: warning
            match res with
            | None ->
                match facts with
                | M.BoundImport (_, _, chain, mN, n) :: _ -> // TODO: warning on tails.
                    r.ResolveTypeNameInChain(chain, S.TN2 (mN, n))
                | _ -> None
            | Some c -> Some (r.Contract(c))

        member r.ResolveTypeName(scope, name) =
            match name with
            | S.TN1 id -> r.ResolveTypeId(scope, id)
            | S.TN2 (mN, id) ->
                match r.ResolveModuleName(scope, mN) with
                | None -> None /// TODO: log error
                | Some m ->
                    let c = M.NamedContract (m, id)
                    if r.IsContract(c) then Some (r.Contract(c)) else None /// TODO: log error

        member r.ResolveTypeNameInChain(chain, name) =
            match chain with
            | M.InitialScope sc -> r.ResolveTypeName(sc, name)
            | M.SubScope (parent, sc) ->
                match r.ResolveTypeName(sc, name) with
                | None -> r.ResolveTypeNameInChain(parent,  name)
                | r -> r

        member r.ResolveTypeQuery(scope, q) =
            // NOTE: guarding against infinite recursion between type-value resolution here.
            if typeQuerySet.Add(scope, q) then
                match q with
                | S.TQ1 id -> r.ResolveVarId(scope, id)
                | S.TQ2 (pQ, id) ->
                    match r.ResolveTypeQuery(scope, pQ) with
                    | None -> None
                    | Some ty ->
                        match ty with
                        | TContract (c, gs) -> r.ResolvePropertyType(c.CKey, id, gs)
                        | _ -> None
            else None

        member r.ResolveTypeQueryInChain(chain, q) =
            match chain with
            | M.InitialScope sc -> r.ResolveTypeQuery(sc, q)
            | M.SubScope (parent, sc) ->
                match r.ResolveTypeQuery(sc, q) with
                | None -> r.ResolveTypeQueryInChain(parent, q)
                | r -> r

        member r.ResolveVarId(scope, id) =
            let facts = r.GetFactsByKey(BoundVarKey (scope, id))
            let res =
                let f =
                    facts
                    |> List.choose (function
                        | M.BoundVar (_, _, t) -> Some t
                        | _ -> None)
                match f with
                | [] -> None
                | [t] -> Some (r.Type(t))
                | t :: _ -> Some (r.Type(t)) // TODO: warning
            match res with
            | None ->
                match facts with
                | M.BoundImport (_, _, chain, mN, n) :: _ -> // TODO: warning on tails.
                    r.ResolveVarNameInChain(chain, S.EN2 (mN, n))
                | _ -> None
            | res -> res

        member r.ResolveVarName(scope, name) =
            match name with
            | S.EN1 id -> r.ResolveVarId(scope, id)
            | S.EN2 (mN, id) ->
                match r.ResolveModuleName(scope, mN) with
                | None -> None /// TODO: log error
                | Some m -> r.ResolveVarId(M.Scope m, id)

        member r.ResolveVarNameInChain(chain, name) =
            match chain with
            | M.InitialScope sc -> r.ResolveVarName(sc, name)
            | M.SubScope (parent, sc) ->
                match r.ResolveVarName(sc, name) with
                | None -> r.ResolveVarNameInChain(parent, name)
                | r -> r

        member r.Run() =
            {
                Contracts = r.GetContracts()
                Values = r.GetValues()
            }

        member r.Signature(s: M.CallSignature) =
            {
                MethodGenerics = s.MethodGenerics
                Parameters = s.Parameters |> List.map r.SignatureParameter
                RestParameter = s.RestParameter |> Option.map r.SignatureParameter
                ReturnType = s.Returns |> Option.map r.Type
            }

        member r.SignatureParameter(p: M.Parameter) : Parameter =
            match p with
            | M.FixedParameter (name, str) -> Sh.ParamConst (name, str)
            | M.NamedParameter (name, ty) -> Sh.Param (name, r.Type(ty))

        member r.Type(t) =
            match t with
            | M.TAny -> TAny
            | M.TArray t -> TArray (r.Type(t))
            | M.TBoolean -> TBoolean
            | M.TContract (c, ts) -> TContract (r.Contract(c), r.Types(ts))
            | M.TGeneric k -> TGeneric k
            | M.TGenericM k -> TGenericM k
            | M.TNamed (chain, tN, ts) ->
                 match r.ResolveTypeNameInChain(chain, tN) with
                 | Some c -> TContract (c, r.Types(ts))
                 | None -> TAny // TODO: log error
            | M.TNumber -> TNumber
            | M.TOf (chain, tQ) ->
                match r.ResolveTypeQueryInChain(chain, tQ) with
                | Some r -> r
                | None -> TAny // TODO: log error
            | M.TString -> TString

        member r.Types(ts) =
            List.map r.Type ts

        static member Create(input) =
            let db = MultiDict.Create(input.Facts, Key.KeysFromFact)
            Resolver(db, input)

    let Resolve input =
        Resolver.Create(input).Run()
