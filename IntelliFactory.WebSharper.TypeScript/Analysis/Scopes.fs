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

(*
    NOTES:
        we are still not considering export A = B.C.D. aliases in modules.
        Does this affect projection .B on a resolved module?
        Also, can module/scope types be somewhat unified?
*)

module Scopes =

    let T<'T> () : NameTable<'T> =
        NameTable()

    let find (x: Dictionary<'T1,'T2>) key =
        let mutable r = Unchecked.defaultof<_>
        if x.TryGetValue(key, &r) then Some r else None

    module Search =

        type Step<'S,'T> =
            | Fail
            | Found of 'T
            | Next of 'S
            | NextThen of 'S * ('T -> 'S)

        let inline withLoopDetection next init =
            let visited = HashSet<'S>()
            let rec on state =
                if visited.Add(state) then None else
                    match next state with
                    | Fail -> None
                    | Found r -> Some r
                    | Next st -> on st
                    | NextThen (st, k) ->
                        match on st with
                        | None -> None
                        | Some r -> on (k r)
            on init

    [<Sealed>]
    type Module(cs, hintPath) =
        member val InternalRoot = Root(cs, hintPath)
        member val ExportedContracts = T<C.Contract>()
        member val ExportedModules = T<Module>()
        member val ExportedValues = T<C.Type>()

    and [<Sealed>] Root(cs: C.Contracts, hintPath: option<NamePath>) =
        member val ContractRegistry = Dictionary<NamePath,C.Contract>()
        member root.Contracts = cs
        member root.HintPath = hintPath
        member val ModuleRegistry = Dictionary<NamePath,Module>()
        member val ScopeRegistry = Dictionary<NamePath,Scope>()

    and [<Sealed>] Scope() =
        member val Contracts = T<C.Contract>()
        member val Modules = T<Module>()
        member val Links = T<S.EntityName>()

    let getOrCreate dict path create =
        match find dict path with
        | None ->
            let r = create ()
            dict.Add(path, r)
            r
        | Some r -> r

    let SubPath (root: Root) path =
        match root.HintPath with
        | None -> path
        | Some bP -> Names.SubPath bP path

    type Root with

        member root.GetOrCreateContract(path) =
            getOrCreate root.ContractRegistry path (fun () ->
                let c = root.Contracts.Contract()
                c.HintPath <- SubPath root path
                c)

        member root.GetOrCreateModule(path) =
            getOrCreate root.ModuleRegistry path (fun () ->
                Module(root.Contracts, Some (SubPath root path)))

        member root.GetOrCreateScope(path) =
            getOrCreate root.ScopeRegistry path (fun () -> Scope())

        member root.IsGlobal =
            root.HintPath.IsNone // TODO: account for external modules

    type Scope with
        member x.BindContract(id, c) = x.Contracts.[id] <- c
        member x.BindModule(id, m) = x.Modules.[id] <- m
        member x.Link(id, name) = x.Links.[id] <- name

    module EntitySearch =
        module Q = Search

        type R =
            | C of C.Contract
            | M of Module

        type T =
            | Fail
            | M1 of Scope * S.Identifier
            | M2 of Scope * S.ModuleName
            | M3 of Module * S.Identifier
            | C1 of Scope * S.Identifier
            | C2 of Module * S.Identifier

        let inline step t =
            match t with
            | Fail -> Q.Fail
            | M1 (scope, id) ->
                match find scope.Modules id with
                | None ->
                    match find scope.Links id with
                    | None -> Q.Fail
                    | Some (S.EN1 id) -> Q.Next (M1 (scope, id))
                    | Some (S.EN2 (name, id)) ->
                        Q.NextThen (M2 (scope, name), function
                            | M m -> M3 (m, id)
                            | _ -> Fail)
                | Some r -> Q.Found (M r)
            | M2 (scope, S.MN1 id) ->
                Q.Next (M1 (scope, id))
            | M2 (scope, S.MN2 (m, id)) ->
                Q.NextThen (M2 (scope, m), function
                    | M m -> M3 (m, id)
                    | _ -> Fail)
            | M3 (m, id) ->
                match find m.ExportedModules id with
                | None -> Q.Fail
                | Some r -> Q.Found (M r)
            | C1 (sc, id) ->
                match find sc.Contracts id with
                | None ->
                    match find sc.Links id with
                    | None -> Q.Fail
                    | Some (S.EN1 id) -> Q.Next (C1 (sc, id))
                    | Some (S.EN2 (m, id)) ->
                        Q.NextThen (M2 (sc, m), function
                            | M m -> C2 (m, id)
                            | _ -> Fail)
                | Some r -> Q.Found (C r)
            | C2 (m, id) ->
                match find m.ExportedContracts id with
                | None -> Q.Fail
                | Some r -> Q.Found (C r)

        let find t = Search.withLoopDetection step t

        let asM x =
            match x with
            | Some (M r) -> Some r
            | _ -> None

        let asC x =
            match x with
            | Some (C r) -> Some r
            | _ -> None

        let findModuleInScope id sc = find (M1 (sc, id)) |> asM
        let findSubModule id mo = find (M3 (mo, id)) |> asM
        let findContractInScope id sc = find (C1 (sc, id)) |> asC
        let findContractInModule id mo = find (C2 (mo, id)) |> asC

    [<Sealed>]
    type ScopeChain(scopes: list<Scope>) =

        new () = ScopeChain([])

        member c.Add(scope) =
            ScopeChain(scope :: scopes)

        member c.ResolveModule(m: S.ModuleName) =
            match m with
            | S.MN1 n -> List.tryPick (EntitySearch.findModuleInScope n) scopes
            | S.MN2 (parent, name) ->
                match c.ResolveModule(parent) with
                | None -> None
                | Some m -> EntitySearch.findSubModule name m

        member c.ResolveContract(t: S.TypeName) : option<C.Contract> =
            match t with
            | S.TN1 id -> List.tryPick (EntitySearch.findContractInScope id) scopes
            | S.TN2 (m, name) ->
                match c.ResolveModule(m) with
                | None -> None
                | Some m -> EntitySearch.findContractInModule name m

        member c.ResolveType(t, ts) =
            C.TLazy <| lazy match c.ResolveContract(t) with
                            | None -> C.TAny
                            | Some tN -> C.TNamed (tN, ts)
