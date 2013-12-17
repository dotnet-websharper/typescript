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
module D = SourceFileDependencies
module E = ExternalModuleNames

/// First-pass traversal of parsed syntax to discover modules and contracts
/// and construct scope structure for later name resolution.
module internal Analysis =

    type Context =
        {
            CurrentModule : Sc.Module
            ExportedRoot : Sc.Root
            ExportedScope : Sc.Scope
            Generics : S.Identifier []
            GenericsM : S.Identifier []
            LocalRoot : Sc.Root
            LocalScope : Sc.Scope
            Path : option<NamePath>
            ScopeChain : Sc.ScopeChain
        }

        member x.SubPath(n) =
            match x.Path with
            | None -> Names.NP1 n
            | Some p -> Names.NP2 (p, n)

    [<Sealed>]
    type Visit(ctx: Context) as self =

        let ( ! ) t = self.Type(t)

        member this.Anonynmous() =
            let c = C.Contract()
            match ctx.Path with
            | None -> ()
            | Some p -> c.HintPath <- Names.NP2 (p, c.HintPath.Name)
            c

        member this.BuildContract(c: C.Contract, ms: list<_>) =
            for body in ms do
                match body with
                | S.TM1 (S.Prop (name, ty)) -> c.AddProp(name, !ty)
                | S.TM1 (S.OptProp (name, ty)) -> c.AddOptProp(name, !ty)
                | S.TM2 callSig ->
                    for s in this.BuildSignatures(ctx, callSig) do
                        c.AddCall(s)
                | S.TM3 newSig ->
                    for s in this.BuildSignatures(ctx, newSig) do
                        c.AddNew(s)
                | S.TM4 (S.ByNumber (id, ty)) -> c.AddByNumber(id, !ty)
                | S.TM4 (S.ByString (id, ty)) -> c.AddByString(id, !ty)

        member this.BuildSignatures(ctx, S.CS (tps, par, ret)) =
            let this =
                match tps with
                | [] -> this
                | ps ->
                    let gs = List.map this.TypeParam tps
                    Visit({ctx with GenericsM = Seq.toArray gs })
            let retTy =
                match ret with
                | S.TVoid -> None
                | ty -> Some !ty
            let makeSig ps rest : C.Signature =
                {
                    MethodGenerics = []
                    Parameters = ps
                    RestParameter = rest
                    ReturnType = retTy
                }
            let convP p =
                match p with
                | S.P1 (name, t) -> C.Parameter.Param (name, !t)
                | S.P2 (name, v) -> C.Parameter.ParamConst (name, v)
            let makeSigs ps opts rest =
                let req = List.map convP ps
                let opt = List.map convP opts
                List.inits opt
                |> List.map (fun opt -> makeSig (req @ opt) rest)
            match par with
            | S.Ps1 ps -> [makeSig (List.map convP ps) None]
            | S.Ps2 (ps, opts) -> makeSigs ps opts None
            | S.Ps3 (ps, opts, rest) -> makeSigs ps opts (Some (convP rest))

        member this.Class(c: S.AmbientClassDeclaration) =
            () // TODO

        member this.Enum(e: S.AmbientEnumDeclaration) =
            () // TODO

        member this.Import(exp, S.ID (id, name)) =
            ctx.LocalScope.Link(id, name)
            match exp with
            | S.Export -> ctx.ExportedScope.Link(id, name)
            | S.NoExport -> ()

        member this.Interface(exp, i: S.InterfaceDeclaration) =
            let (root, path) =
                match exp with
                | S.Export -> (ctx.ExportedRoot, ctx.SubPath i.InterfaceName)
                | S.NoExport -> (ctx.LocalRoot, Names.NP1 i.InterfaceName)
            let c = root.GetOrCreateContract(path)
            let vis =
                match i.InterfaceTypeParameters with
                | [] -> this
                | ps ->
                    let tps = List.map this.TypeParam ps
                    c.SetGenerics(tps)
                    Visit({ ctx with Generics = Seq.toArray tps })
            vis.BuildContract(c, i.InterfaceBody)
            for r in i.InterfaceExtends do
                c.Extend(this.TypeRef(r))
            ctx.LocalScope.BindContract(i.InterfaceName, c)
            match exp with
            | S.Export ->
                ctx.ExportedScope.BindContract(i.InterfaceName, c)
                ctx.CurrentModule.ExportedContracts.Add(i.InterfaceName, c)
            | S.NoExport -> ()

        member this.Module(exp, (S.AMD (id, body) as mdecl)) =
            let (root, path) =
                match exp with
                | S.Export -> (ctx.ExportedRoot, ctx.SubPath id)
                | S.NoExport -> (ctx.LocalRoot, Names.NP1 id)
            let mo = root.GetOrCreateModule(path)
            ctx.LocalScope.BindModule(id, mo)
            match exp with
            | S.Export -> ctx.ExportedScope.BindModule(id, mo)
            | S.NoExport -> ()
            let locScope = Sc.Scope()
            let expScope = root.GetOrCreateScope(path)
            let subContext =
                {
                    CurrentModule = mo
                    Generics = ctx.Generics
                    GenericsM = ctx.GenericsM
                    ExportedRoot = root
                    ExportedScope = expScope
                    LocalRoot = mo.InternalRoot
                    LocalScope = locScope
                    Path = Some path
                    ScopeChain = ctx.ScopeChain.Add(expScope).Add(locScope)
                }
            let v = Visit(subContext)
            for el in body.List do
                v.ModuleElement(el)
            // TODO: instantiated modules bind a variable

        member this.ModuleElement(el) =
            match el with
            | S.AME1 (S.AVD (id, ty)) ->
                this.Var(S.Export, id, ty)
            | S.AME2 (S.AFD (id, cSig)) ->
                this.Var(S.Export, id, S.TObject [S.TM2 cSig])
            | S.AME3 c -> this.Class(c)
            | S.AME4 i -> this.Interface(S.Export, i)
            | S.AME5 e -> this.Enum(e)
            | S.AME6 md -> this.Module(S.Export, md)
            | S.AME7 (em, imp) -> this.Import(em, imp)

        member this.SourceFile(sf: D.SourceFile) =
            () // TODO

        member this.Type(t) =
            match t with
            | S.TAny -> C.TAny
            | S.TNumber -> C.TNumber
            | S.TBoolean -> C.TBoolean
            | S.TString -> C.TString
            | S.TVoid -> C.TAny
            | S.TReference (S.TRef (tN, gs)) ->
                let inline def () =
                    ctx.ScopeChain.ResolveType(tN, List.map this.Type gs)
                match gs with
                | [] ->
                    match Array.IndexOf(ctx.Generics, tN) with
                    | -1 ->
                        match Array.IndexOf(ctx.GenericsM, tN) with
                        | -1 -> def ()
                        | i -> C.TGenericM i
                    | i -> C.TGeneric i
                | _ -> def ()
            | S.TQuery tQ ->
                failwith "TODO: TypeQuery"
            | S.TArray t -> C.TArray (this.Type t)
            | S.TObject ms ->
                let c = this.Anonynmous()
                this.BuildContract(c, ms)
                C.TNamed (c, [])

        member this.TypeParam(tP) =
            match tP with
            | S.TP1 x -> x
            | S.TP2 (x, y) -> x // ignoring constraint for now

        member this.TypeRef(S.TRef (tN, args)) =
            ctx.ScopeChain.ResolveType(tN, List.map (!) args)

        member this.Var(exp, id, ty) =
            match exp with
            | S.Export -> ctx.CurrentModule.ExportedValues.Add(id, !ty)
            | _ -> ()
            // TODO: record more information so that type-queries may work.

    let createGlobalContext () =
        let globalModule = Sc.Module()
        let globalRoot = globalModule.InternalRoot
        let globalScope = Sc.Scope()
        {
            CurrentModule = globalModule
            ExportedRoot = globalRoot
            ExportedScope = globalScope
            Generics = Array.empty
            GenericsM = Array.empty
            LocalRoot = globalRoot
            LocalScope = globalScope
            Path = None
            ScopeChain = Sc.ScopeChain().Add(globalScope)
        }

    [<Sealed>]
    type Value(path: NamePath, ty: C.Type) =
        member v.NamePath = path
        member v.Type = ty

    type Input =
        {
            SourceFiles : seq<D.SourceFile>
        }

    type Output =
        {
            Contracts : seq<C.Contract>
            Values : seq<Value>
        }

    let Analyze input =
        let ctx = createGlobalContext ()
        let vis = Visit(ctx)
        for sF in input.SourceFiles do
            vis.SourceFile(sF)
        // TODO: force all type resolution suspentions..
        // TODO: actually collect values and contracts.
        {
            Contracts = Seq.empty
            Values = Seq.empty
        }
