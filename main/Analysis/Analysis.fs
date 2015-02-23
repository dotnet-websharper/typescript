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

module C = Contracts
module S = Syntax
module Sc = Scopes
module D = SourceFileDependencies
module E = ExternalModuleNames

/// First-pass traversal of parsed syntax to discover modules and contracts
/// and construct scope structure for later name resolution.
module internal Analysis =

    [<Sealed>]
    type Value private (hintPath: NamePath, namePath: NamePath, ty: C.Type) =

        static member Create(namePath, ty, hintPath) =
            Value(hintPath, namePath, ty)

        static member Create(namePath, ty) =
            Value(namePath, namePath, ty)

        member v.HintPath = hintPath
        member v.NamePath = namePath
        member v.Type = ty

    type ValueBuilder =
        {
            AllValues : ResizeArray<Value>
        }

        member vb.Value(path, ty) =
            let v = Value.Create(path, ty)
            vb.AllValues.Add(v)
            v

        member vb.Value(path, ty, hintPath) =
            let v = Value.Create(path, ty, hintPath)
            vb.AllValues.Add(v)
            v

        member vb.All =
            vb.AllValues :> seq<_>

        static member Create() =
            {
                AllValues = ResizeArray()
            }

    type SharedNames =
        {
            CName : Name
            MiscName : Name
            TName : Name
        }

    type State =
        {
            Contracts : C.Contracts
            Logger : Logger
            NameBuilder : Names.NameBuilder
            SharedNames : SharedNames
            ValueBuilder : ValueBuilder
        }

        static member Create(logger, names) =
            {
                Contracts = C.Contracts()
                Logger = logger
                NameBuilder = names
                SharedNames =
                    {
                        CName = names.CreateName("Create")
                        MiscName = names.CreateName("Misc")
                        TName = names.CreateName("T")
                    }
                ValueBuilder = ValueBuilder.Create()
            }

        member st.AnonymousContract(hintPath) =
            st.Contracts.AnonymousContract(hintPath)

        member st.NamedContract(hintPath) =
            st.Contracts.NamedContract(hintPath)

    type Context =
        {
            AnonNameHint : option<Name>
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
    type Visit(st: State, ctx: Context) =

        member this.AnonContract(hintName) =
            let parent =
                match ctx.Path with
                | None -> Names.NP1 st.SharedNames.MiscName
                | Some p -> Names.NP2 (p, st.SharedNames.MiscName)
            let hintPath = Names.NP2 (parent, hintName)
            st.AnonymousContract(hintPath)

        member this.AnonType(ms) =
            let gs = Array.append ctx.Generics ctx.GenericsM
            let self = Visit(st, { ctx with Generics = gs; GenericsM = Array.empty })
            let nameHint =
                match ctx.AnonNameHint with
                | None -> st.SharedNames.TName
                | Some hint -> hint
            let c = self.AnonContract(nameHint)
            if gs.Length > 0 then
                c.SetGenerics(List.ofArray gs)
            self.BuildContract(c, ms)
            let gArgs =
                let l1 = ctx.Generics.Length
                let l2 = ctx.GenericsM.Length
                List.init (l1 + l2) (fun i ->
                    if i < l1 then C.TGeneric i else C.TGenericM (i - l1))
            C.TNamed (c, gArgs)

        member this.BuildContract(c: C.Contract, ms: list<_>) =
            for body in ms do
                match body with
                | S.TM1 prop ->
                    this.BuildProperty(c, prop)
                | S.TM2 callSig ->
                    for s in this.BuildSignatures(ctx, callSig) do
                        c.AddCall(s)
                | S.TM3 newSig ->
                    for s in this.BuildSignatures(ctx, newSig) do
                        c.AddNew(s)
                | S.TM4 (S.ByNumber (id, ty)) -> c.AddByNumber(id, this.Type(ty))
                | S.TM4 (S.ByString (id, ty)) -> c.AddByString(id, this.Type(ty))

        member this.BuildProperty(c: C.Contract, prop: S.PropertySignature) =
            let (name, ty, isOpt) =
                match prop with
                | S.Prop (name, ty) -> (name, ty, false)
                | S.OptProp (name, ty) -> (name, ty, true)
            let this = Visit(st, { ctx with AnonNameHint = Some name })
            let def () =
                let ty = this.Type(ty)
                if isOpt
                    then c.AddOptProp(name, ty)
                    else c.AddProp(name, ty)
            match ty with
            | S.TObject ms ->
                match C.TryGetAnonymousPropertyContract c name with
                | None -> def ()
                | Some res -> this.BuildContract(res, ms)
            | _ -> def ()

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
            this.Interface(S.Export, this.ClassToInterface(c), tSuffix = true)
            this.Module(S.Export, this.ClassToStatics(c))

        member this.ClassToInterface(c: S.AmbientClassDeclaration) : S.InterfaceDeclaration =
            {
                InterfaceName = c.ClassName
                InterfaceTypeParameters = c.ClassTypeParameters
                InterfaceExtends = Option.toList c.ClassExtends @ c.ClassImplements
                InterfaceBody =
                    [
                        for m in c.ClassBody do
                            match m with
                            | S.ClassConstructor _ -> ()
                            | S.ClassIndex iS ->
                                yield S.TM4 iS
                            | S.ClassMethod (acc, sc, name, sign) ->
                                match acc, sc with
                                | S.Public, S.Instance ->
                                    yield S.TM1 (S.Prop (name, S.TObject [S.TM2 sign]))
                                | _ -> ()
                            | S.ClassProperty (acc, sc, name, ty) ->
                                match acc, sc with
                                | S.Public, S.Instance ->
                                    yield S.TM1 (S.Prop (name, ty))
                                | _ -> ()
                    ]
            }

        member this.ClassToStatics(s: S.AmbientClassDeclaration) : S.AmbientModuleDeclaration =
            let selfTP = s.ClassTypeParameters
            let mkT (t: S.TypeParameter) =
                match t with
                | S.TP1 n
                | S.TP2 (n, _) -> S.TReference (S.TRef (S.TN1 n, []))
            let selfType = S.TReference (S.TRef (S.TN1 s.ClassName, List.map mkT selfTP))
            do
                let ctors =
                    [
                        for m in s.ClassBody do
                            match m with
                            | S.ClassConstructor ps -> yield ps
                            | _ -> ()
                    ]
                match ctors with
                | [] -> ()
                | _ ->
                    let contract =
                        S.TObject [
                            for ps in ctors ->
                                S.TM3 (S.CS (selfTP, ps, selfType))
                        ]
                    this.Var(S.Export, s.ClassName, contract, ctorSuffix = true)
            let elements : list<S.AmbientModuleElement> =
                [
                    for m in s.ClassBody do
                        match m with
                        | S.ClassConstructor _ -> ()
                        | S.ClassIndex _ -> ()
                        | S.ClassMethod (acc, sc, name, sign) ->
                            match acc, sc with
                            | S.Public, S.Static ->
                                yield S.AME2 (S.AFD (name, sign))
                            | _ -> ()
                        | S.ClassProperty (acc, sc, name, ty) ->
                            match acc, sc with
                            | S.Public, S.Static ->
                                yield S.AME1 (S.AVD (name, ty))
                            | _ -> ()
                ]
            S.AMD (s.ClassName, elements)

        member this.Enum(e: S.AmbientEnumDeclaration) =
            let c = this.Interface(S.Export, this.EnumToInterface(e))
            this.Var(S.Export, e.EnumName, this.EnumToStatics(e))

        member this.EnumToInterface(e: S.AmbientEnumDeclaration) : S.InterfaceDeclaration =
            {
                InterfaceName = e.EnumName
                InterfaceTypeParameters = []
                InterfaceExtends = []
                InterfaceBody = []
            }

        member this.EnumToStatics(e: S.AmbientEnumDeclaration) : S.Type =
            let selfType = S.TReference (S.TRef (S.TN1 e.EnumName, []))
            S.TObject [
                for m in e.EnumBody do
                    match m with
                    | S.AEM1 name
                    | S.AEM2 (name, _) -> // TODO: ignoring int value for now
                        yield S.TM1 (S.Prop (name, selfType))
            ]

        member this.ExportContract(exp, name: Name, c: C.Contract) =
            match exp with
            | S.Export ->
                ctx.ExportedScope.BindContract(name, Sc.Local c)
                ctx.CurrentModule.ExportedContracts.[name] <- Sc.Local c
            | S.NoExport -> ()

        member this.ExportModule(exp, name: Name, mo: Sc.Module) =
            match exp with
            | S.Export ->
                ctx.ExportedScope.BindModule(name, mo)
                ctx.CurrentModule.ExportedModules.[name] <- mo
            | S.NoExport -> ()

        member this.Import(exp, S.ID (id, name)) =
            ctx.LocalScope.Link(id, name)
            match exp with
            | S.Export -> ctx.ExportedScope.Link(id, name)
            | S.NoExport -> ()

        member this.Interface(exp, i: S.InterfaceDeclaration, tSuffix: bool) =
            let (root, path) =
                match exp with
                | S.Export ->
                    (ctx.ExportedRoot, ctx.SubPath i.InterfaceName)
                | S.NoExport ->
                    (ctx.LocalRoot, Names.NP1 i.InterfaceName)
            let c = root.GetOrCreateNamedContract(path, ?suffix = if tSuffix then Some st.SharedNames.TName else None)
            match c with
            | Sc.Local c ->
                let vis =
                    match i.InterfaceTypeParameters with
                    | [] -> this
                    | ps ->
                        let tps = List.map this.TypeParam ps
                        c.SetGenerics(tps)
                        Visit(st, { ctx with Generics = Seq.toArray tps })
                vis.BuildContract(c, i.InterfaceBody)
                for r in i.InterfaceExtends do
                    c.Extend(vis.TypeRef(r))
                ctx.LocalScope.BindContract(i.InterfaceName, Sc.Local c)
                this.ExportContract(exp, i.InterfaceName, c)
            | _ ->
                st.Logger.Exception (Failure "Conflict between local and foreign contract")

        member this.Interface(exp, i) =
            this.Interface(exp, i, false)

        member this.Module(exp, (S.AMD (id, body) as mdecl)) =
            let (root, path) =
                match exp with
                | S.Export -> (ctx.ExportedRoot, ctx.SubPath id)
                | S.NoExport -> (ctx.LocalRoot, Names.NP1 id)
            let mo = root.GetOrCreateModule(path)
            ctx.LocalScope.BindModule(id, mo)
            this.ExportModule(exp, id, mo)
            let locScope = Sc.Scope()
            let expScope = root.GetOrCreateScope(path)
            let subContext =
                {
                    AnonNameHint = None
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
            let v = Visit(st, subContext)
            for el in body do
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
            for decl in decls do
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

        member this.Type(t: S.Type) =
            match t with
            | S.TAny -> C.TAny
            | S.TNumber -> C.TNumber
            | S.TBoolean -> C.TBoolean
            | S.TString -> C.TString
            | S.TVoid -> C.TAny
            | S.TReference (S.TRef (tN, gs)) ->
                let inline def () =
                    ctx.ScopeChain.ResolveType(tN, List.map this.Type gs)
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
                C.TAny
                // failwith "TODO: TypeQuery"
            | S.TArray t -> C.TArray (this.Type t)
            | S.TObject ms -> this.AnonType(ms)

        member this.TypeParam(tP) =
            match tP with
            | S.TP1 x -> x
            | S.TP2 (x, y) -> x /// TODO: ignoring constraint for now

        member this.TypeRef(S.TRef (tN, args)) =
            ctx.ScopeChain.ResolveType(tN, List.map this.Type args)

        member this.Var(exp, id, ty, ctorSuffix: bool) =
            /// TODO: merging anon types on duplicate `function` decls.
            match exp with
            | S.Export ->
                let def () =
                    let vs = ctx.CurrentModule.ExportedValues
                    let ty = this.Type(ty)
                    vs.[id] <- ty
                    if ctx.ExportedRoot.IsGlobal then // TODO: account for external modules
                        let p = ctx.SubPath(id)
                        if ctorSuffix then
                            st.ValueBuilder.Value(p, ty, NamePath.NP2 (p, st.SharedNames.CName)) |> ignore
                        else
                            st.ValueBuilder.Value(p, ty) |> ignore
                match ctx.CurrentModule.ExportedValues.TryGetValue(id) with
                | true, C.TNamed (c, _) ->
                    match ty with
                    | S.TObject ms -> this.BuildContract(c, ms)
                    | _ -> def ()
                | _ -> def ()
            | _ -> ()
            // TODO: record more information so that type-queries may work.

        member this.Var(exp, id, ty) =
            this.Var(exp, id, ty, false)

    let createGlobalContext (metaTable: Metadata.Table) st =
        let globalModule = Sc.Module(st.Contracts, None)
        let globalRoot = globalModule.InternalRoot
        let globalScope = Sc.Scope()
        metaTable.Install(globalRoot, globalScope)
        {
            AnonNameHint = None
            CurrentModule = globalModule
            ExportedRoot = globalRoot
            ExportedScope = globalScope
            Generics = Array.empty
            GenericsM = Array.empty
            LocalRoot = globalRoot
            LocalScope = globalScope
            Path = None
            ScopeChain = Sc.ScopeChain(st.Logger).Add(globalScope)
        }

    type Input =
        {
            Logger : Logger
            MetadataTable : Metadata.Table
            NameBuilder : Names.NameBuilder
            SourceFiles : seq<D.SourceFile>
        }

    type Output =
        {
            Contracts : seq<C.Contract>
            Values : seq<Value>
        }

    let Analyze input =
        let st = State.Create(input.Logger, input.NameBuilder)
        let ctx = createGlobalContext input.MetadataTable st
        let vis = Visit(st, ctx)
        for sF in input.SourceFiles do
            vis.SourceFile(sF)
        {
            Contracts = st.Contracts.All
            Values = st.ValueBuilder.All
        }
