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

    [<NoComparison>]
    [<ReferenceEquality>]
    type Value =
        {
            ValuePath : NamePath
            ValueType : C.Type
        }

        member v.NamePath = v.ValuePath
        member v.Type = v.ValueType

    type ValueBuilder =
        {
            AllValues : ResizeArray<Value>
        }

        member vb.Value(path, ty) =
            let v = { ValuePath = path; ValueType = ty }
            vb.AllValues.Add(v)
            v

        member vb.All =
            vb.AllValues :> seq<_>

        static member Create() =
            {
                AllValues = ResizeArray()
            }

    type State =
        {
            Contracts : C.Contracts
            ValueBuilder : ValueBuilder
        }

        static member Create() =
            {
                Contracts = C.Contracts()
                ValueBuilder = ValueBuilder.Create()
            }

        member st.Contract() =
            st.Contracts.Contract()

    type Context =
        {
            CurrentModule : Sc.Module
            ExportedRoot : Sc.Root
            ExportedScope : Sc.Scope
            IsInsideType : bool
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
    type Visit(st: State, ctx: Context) =

        member this.AnonContract() =
            let c = st.Contract()
            match ctx.Path with
            | None -> ()
            | Some p -> c.HintPath <- Names.NP2 (p, c.HintPath.Name (* Anon *))
            c

        member this.BuildContract(c: C.Contract, ms: list<_>) =
            let inline ( ! ) t = this.Type t
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
            let (this, gs) =
                match tps with
                | [] -> (this, [])
                | ps ->
                    let gs = List.map this.TypeParam tps
                    (Visit(st, {ctx with GenericsM = Seq.toArray gs }), gs)
            let retTy =
                match ret with
                | S.TVoid -> None
                | ty -> Some (this.Type ty)
            let makeSig ps rest : C.Signature =
                {
                    MethodGenerics = gs
                    Parameters = ps
                    RestParameter = rest
                    ReturnType = retTy
                }
            let convP p =
                match p with
                | S.P1 (name, t) -> C.Parameter.Param (name, this.Type t)
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
            c.MarkNamedUse()
            let vis =
                match i.InterfaceTypeParameters with
                | [] -> this
                | ps ->
                    let tps = List.map this.TypeParam ps
                    c.SetGenerics(tps)
                    Visit(st, { ctx with Generics = Seq.toArray tps })
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
                    IsInsideType = ctx.IsInsideType
                    ExportedRoot = root
                    ExportedScope = expScope
                    LocalRoot = mo.InternalRoot
                    LocalScope = locScope
                    Path = Some path
                    ScopeChain = ctx.ScopeChain.Add(expScope).Add(locScope)
                }
            let v = Visit(st, subContext)
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
            // TODO: recognize if this is an external module or a normal file?
            // For now, only deal with normal files.
            let decls =
                match sf.Syntax with
                | S.DSF decls -> decls
            for decl in decls.List do
                match decl with
                | S.DE1 _ -> () // export assignments not possible in normal source files
                | S.DE2 (_, i) -> // no export modifier in normal source files
                    this.Interface(S.Export, i)
                | S.DE3 (_, imp) -> // no export modifier in normal source files
                    this.Import(S.Export, imp)
                | S.DE4 _ -> () // TODO: external imports are not yet suported
                | S.DE5 (_, decl) -> // no export modifier in normal source files
                    let em = S.Export
                    match decl with
                    | S.AD1 (S.AVD (id, ty)) -> this.Var(em, id, ty)
                    | S.AD2 (S.AFD (id, s)) -> this.Var(em, id, S.TObject [S.TM2 s])
                    | S.AD3 c -> this.Class(c)
                    | S.AD4 e -> this.Enum(e)
                    | S.AD5 m -> this.Module(em, m)
                    | S.AD6 _ -> () // TODO: ambient external modules

        member this.Type(t) =
            let self = if ctx.IsInsideType then this else this.WithInsideType()
            match t with
            | S.TAny -> C.TAny
            | S.TNumber -> C.TNumber
            | S.TBoolean -> C.TBoolean
            | S.TString -> C.TString
            | S.TVoid -> C.TAny
            | S.TReference (S.TRef (tN, gs)) ->
                let inline def () =
                    let ty = ctx.ScopeChain.ResolveType(tN, List.map self.Type gs)
                    if ctx.IsInsideType then
                        match ty with
                        | C.TLazy c ->
                            C.TLazy <| lazy
                                match c.Value with
                                | C.TNamed (con, _) ->
                                    match con.Kind with
                                    | Shapes.MethodContract _ -> con.MarkNamedUse()
                                    | _ -> ()
                                | _ -> ()
                                c.Value
                        | r -> r
                    else ty
                match gs, tN with
                | [], S.TN1 t ->
                    match Array.IndexOf(ctx.Generics, t) with
                    | -1 ->
                        match Array.IndexOf(ctx.GenericsM, t) with
                        | -1 -> def ()
                        | i -> C.TGenericM i
                    | i -> C.TGeneric i
                | _ -> def ()
            | S.TQuery tQ ->
                failwith "TODO: TypeQuery"
            | S.TArray t -> C.TArray (self.Type t)
            | S.TObject ms ->
                let c = self.AnonContract()
                self.BuildContract(c, ms)
                C.TNamed (c, [])

        member this.TypeParam(tP) =
            match tP with
            | S.TP1 x -> x
            | S.TP2 (x, y) -> x // ignoring constraint for now

        member this.TypeRef(S.TRef (tN, args)) =
            ctx.ScopeChain.ResolveType(tN, List.map this.Type args)

        member this.Var(exp, id, ty) =
            let ty = this.Type ty
            match exp with
            | S.Export ->
                ctx.CurrentModule.ExportedValues.Add(id, ty)
                if ctx.ExportedRoot.IsGlobal then // TODO: account for external modules
                    st.ValueBuilder.Value(ctx.SubPath(id), ty) |> ignore
            | _ -> ()
            // TODO: record more information so that type-queries may work.

        member this.WithInsideType() =
            Visit(st, { ctx with IsInsideType = true })

    let createGlobalContext st =
        let globalModule = Sc.Module(st.Contracts, None)
        let globalRoot = globalModule.InternalRoot
        let globalScope = Sc.Scope()
        {
            CurrentModule = globalModule
            ExportedRoot = globalRoot
            ExportedScope = globalScope
            Generics = Array.empty
            GenericsM = Array.empty
            IsInsideType = false
            LocalRoot = globalRoot
            LocalScope = globalScope
            Path = None
            ScopeChain = Sc.ScopeChain().Add(globalScope)
        }

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
        let st = State.Create()
        let ctx = createGlobalContext st
        let vis = Visit(st, ctx)
        for sF in input.SourceFiles do
            vis.SourceFile(sF)
        {
            Contracts = st.Contracts.All
            Values = st.ValueBuilder.All
        }
