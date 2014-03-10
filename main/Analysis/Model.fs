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

module D = SourceFileDependencies
module S = Syntax

/// Fact-based model and databse for TypeScript declaration fragment.
module (* internal *) Model =

    /// An object with reference equality identity of a given inner object,
    /// even if the inner object overrides `.Equals` and `.GetHashCode`.
    [<Sealed>]
    type Box(x: obj) =
        let hash = RuntimeHelpers.GetHashCode(x)

        new () = Box(obj())


        override box.Equals(other: obj) =
            match other with
            | :? Box as o -> Object.ReferenceEquals(x, o.V)
            | _ -> false

        override box.GetHashCode() =
            hash

        override box.ToString() =
            String.Format("box<{0:x}>", hash)

        member private box.V = x

    /// Module identity.
    type Module =

        /// An external module, given a unique identity.
        | ExternalModule of Box // Box = unique identity, probably resolved external module name (TODO)

        /// The implied global module.
        | GlobalModule

        /// Local, non-exported module with a unique identity.
        /// In the `.d.ts` fragment, these are only possible inside
        /// `AmbientExternalModuleDeclaration` with `ExportAssignment`,
        /// when non-exported.
        | PrivateModule of Box // obj = unique identity, such as the syntactic declaration.

        /// Sub-module accessible from a given parent module.
        | SubModule of Module * Name

    /// Scopes that determine name accessibility.
    type Scope =
        | Scope of Module

        // member sc.Module = let (Scope m) = sc in m

    /// Contract identity.
    type Contract =

        /// Anonymous contract is represented by its syntactic entity.
        | AnonymousContract of Box

        /// A named contract is represented as a parent module and a unique name.
        | NamedContract of Module * Name

        /// Contract associated with a property of another contract.
        | PropertyContract of Contract * Name

        /// Contract associated with a variable in a module.
        | VariableContract of Scope * Name

    type ScopeChain =
        | InitialScope of Scope
        | SubScope of ScopeChain * Scope

        member chain.With(scope) =
            SubScope (chain, scope)

    type Type =
        | TAny
        | TArray of Type
        | TBoolean
        | TContract of Contract * list<Type>
        | TGeneric of int
        | TGenericM of int
        | TNamed of ScopeChain * S.TypeName * list<Type>
        | TNumber
        | TOf of ScopeChain * S.TypeQuery
        | TString

    type Parameter =
        | FixedParameter of Name * string
        | NamedParameter of Name * Type

    type CallSignature =
        {
            CSMethodGenerics : list<Name>
            CSParameters : list<Parameter>
            CSRestParameter : option<Parameter>
            CSReturns : option<Type>
        }

        member cs.MethodGenerics = cs.CSMethodGenerics
        member cs.Parameters = cs.CSParameters
        member cs.RestParameter = cs.CSRestParameter
        member cs.Returns = cs.CSReturns

    type Value =
        {
            HintPath : NamePath
            NamePath : NamePath
            Type : Type
        }

    type ContractFlags =
        | IsAnonymous = 1
        | None = 0

    type ContractInfo =
        {
            CIExtends : list<Type>
            CIFlags : ContractFlags
            CIGenerics : list<Name>
            CIHintPath : NamePath
        }

        member ci.Extends = ci.CIExtends
        member ci.Flags = ci.CIFlags
        member ci.Generics = ci.CIGenerics
        member ci.HintPath = ci.CIHintPath

    type Fact =
        | BoundContract of Scope * Name * Contract
        | BoundImport of Scope * Name * ScopeChain * S.ModuleName * Name
        | BoundModule of Scope * Name * Module
        | BoundModuleImport of Scope * Name * ScopeChain * Name
        | BoundVar of Scope * Name * Type
        | CanCall of Contract * CallSignature
        | CanConstruct of Contract * CallSignature
        | CanIndexByNumber of Contract * Name * Type
        | CanIndexByString of Contract * Name * Type
        | HasProperty of Contract * Name * Type * bool // bool = is-optional
        | IsContract of Contract * ContractInfo
        | IsValue of Value

    type ModuleContext =
        {
            ParentModule : Module
            PrivateScope : Scope
            ScopeChain : ScopeChain
        }

        member ctx.PublicScope = Scope ctx.ParentModule

    type TypeContext =
        {
            TCGenerics : Name []
            TCGenericsM : Name []
            TCHint : option<Name>
            TCScopeChain : ScopeChain
        }

        static member Create(ctx) =
            {
                TCGenerics = Array.empty
                TCGenericsM = Array.empty
                TCHint = None
                TCScopeChain = ctx.ScopeChain
            }

    [<Sealed>]
    type Visit() =

        member v.AmbientClassDeclaration(mctx, tctx, decl: S.AmbientClassDeclaration) =
            v.AmbientClassDynamics(mctx, decl)
            v.AmbientClassStatics(mctx, tctx, decl)

        member v.AmbientClassDynamics(mctx, c: S.AmbientClassDeclaration) =
            let contract : S.InterfaceDeclaration =
                {
                    InterfaceName = c.ClassName
                    InterfaceTypeParameters = c.ClassTypeParameters
                    InterfaceExtends = Option.toList c.ClassExtends @ c.ClassImplements
                    InterfaceBody =
                        [
                            for m in c.ClassBody do
                                match m with
                                | S.ClassIndex iS ->
                                    yield S.TM4 iS
                                | S.ClassMethod (S.Public, S.Instance, name, sign) ->
                                    yield S.TM1 (S.Prop (name, S.TObject [S.TM2 sign]))
                                | S.ClassProperty (S.Public, S.Instance, name, ty) ->
                                    yield S.TM1 (S.Prop (name, ty))
                                | _ -> ()
                        ]
                }
            /// TODO: special name hints, etc?
            v.InterfaceDeclaration(mctx, contract)

        member v.AmbientClassStatics(mctx, tctx, s: S.AmbientClassDeclaration) =
            let selfT =
                let gs =
                    s.ClassTypeParameters
                    |> List.mapi (fun i _ -> S.TGeneric i)
                S.TReference (S.TRef (S.TN1 s.ClassName, gs))
            let ty =
                S.TObject [
                    for m in s.ClassBody do
                        match m with
                        | S.ClassConstructor ps ->
                            yield S.TM3 (S.CS (s.ClassTypeParameters, ps, selfT))
                        | S.ClassMethod (S.Public, S.Static, name, cs) ->
                            yield S.TM1 (S.Prop (name, S.TObject [S.TM2 cs]))
                        | S.ClassProperty (S.Public, S.Static, name, ty) ->
                            yield S.TM1 (S.Prop (name, ty))
                        | _ -> ()
                ]
            /// TODO: name hints, etc?
            v.AmbientVariableDeclaration(mctx, tctx, s.ClassName, ty)

        member v.AmbientEnumDeclaration(mctx, tctx, e: S.AmbientEnumDeclaration) =
            v.AmbientEnumDynamics(mctx, e)
            v.AmbientEnumStatics(mctx, tctx, e)

        member v.AmbientEnumDynamics(mctx, e: S.AmbientEnumDeclaration) =
            let i : S.InterfaceDeclaration =
                {
                    InterfaceName = e.EnumName
                    InterfaceTypeParameters = []
                    InterfaceExtends = []
                    InterfaceBody = []
                }
            v.InterfaceDeclaration(mctx, i)

        member v.AmbientEnumStatics(mctx, tctx, e: S.AmbientEnumDeclaration) =
            let selfType = S.TReference (S.TRef (S.TN1 e.EnumName, []))
            let ty =
                S.TObject [
                    for m in e.EnumBody do
                        match m with
                        | S.AEM1 name
                        | S.AEM2 (name, _) -> // TODO: ignoring int value for now
                            yield S.TM1 (S.Prop (name, selfType))
                ]
            /// TODO: name hints, etc.
            v.AmbientVariableDeclaration(mctx, tctx, e.EnumName, ty)

        member v.AmbientModuleDeclaration(ctx, exported, md) =
            let box = Box md
            let (S.AMD (id, body)) = md
            let thisModule =
                if exported then
                    SubModule (ctx.ParentModule, id)
                else
                    PrivateModule box
            let privateScope = Scope (PrivateModule box)
            let scopeChain =
                if exported then
                    let publicScope = Scope thisModule
                    ctx.ScopeChain.With(publicScope).With(privateScope)
                else
                    ctx.ScopeChain.With(privateScope)
            let sub =
                {
                    ctx with
                        ParentModule = thisModule
                        PrivateScope = privateScope
                        ScopeChain = scopeChain
                }
            if exported then
                BoundModule (ctx.PublicScope, id, thisModule)
                |> v.YieldFact
            BoundModule (ctx.PrivateScope, id, thisModule)
            |> v.YieldFact
            let tctx = TypeContext.Create(sub)
            for el in body do
                v.AmbientModuleElement(sub, tctx, el)
            /// TODO: bind a value if module is instantiated.

        member v.AmbientModuleDeclaration(ctx, md) =
            v.AmbientModuleDeclaration(ctx, true, md)

        member v.AmbientModuleElement(mctx, tctx, el) =
            match el with
            | S.AME1 decl -> v.AmbientVariableDeclaration(mctx, tctx, decl)
            | S.AME2 (S.AFD (name, cSig)) -> v.AmbientVariableDeclaration(mctx, tctx, name, S.TObject [S.TM2 cSig])
            | S.AME3 classDecl -> v.AmbientClassDeclaration(mctx, tctx, classDecl)
            | S.AME4 decl -> v.InterfaceDeclaration(mctx, decl)
            | S.AME5 enumDecl -> v.AmbientEnumDeclaration(mctx, tctx, enumDecl)
            | S.AME6 decl -> v.AmbientModuleDeclaration(mctx, decl)
            | S.AME7 (exp, impDecl) -> v.ImportDecalration(mctx, exp, impDecl)

        member v.AmbientVariableDeclaration(mctx, tctx, exported, id, ty) =
            let scope = if not exported then mctx.PrivateScope else mctx.PublicScope
            let ty = v.Type({ tctx with TCHint = Some id }, ty, VariableContract (scope, id))
            if exported then
                BoundVar (mctx.PublicScope, id, ty)
                |> v.YieldFact
            BoundVar (mctx.PrivateScope, id, ty)
            |> v.YieldFact

        member v.AmbientVariableDeclaration(mctx, tctx, (S.AVD (id, ty))) =
            v.AmbientVariableDeclaration(mctx, tctx, true, id, ty)

        member v.AmbientVariableDeclaration(mctx, tctx, id, ty) =
            v.AmbientVariableDeclaration(mctx, tctx, true, id, ty)

        member v.ImportDecalration(mctx, exp, imp) =
            let scopes =
                match exp with
                | S.Export -> [mctx.PrivateScope; mctx.PublicScope]
                | S.NoExport -> [mctx.PublicScope]
            for scope in scopes do
                match imp with
                | S.ID (id, S.EN1 name) ->
                    BoundModuleImport (scope, id, mctx.ScopeChain, name)
                | S.ID (id, S.EN2 (mN, name)) ->
                    BoundImport (scope, id, mctx.ScopeChain, mN, name)
                |> v.YieldFact

        member v.InterfaceDeclaration(ctx, exported: bool, decl: S.InterfaceDeclaration) =
            let t = NamedContract (ctx.ParentModule, decl.InterfaceName)
            if exported then
                BoundContract (ctx.PublicScope, decl.InterfaceName, t)
                |> v.YieldFact
            BoundContract (ctx.PrivateScope, decl.InterfaceName, t)
            |> v.YieldFact
            let tc =
                {
                    TCGenerics = v.TypeParameters(decl.InterfaceTypeParameters)
                    TCGenericsM = [||]
                    TCScopeChain = ctx.ScopeChain
                    TCHint = Some decl.InterfaceName
                }
            let info : ContractInfo =
                {
                    CIExtends = [ for tR in decl.InterfaceExtends -> v.Type(tc, S.TReference tR) ]
                    CIFlags = ContractFlags.None
                    CIGenerics = List.ofArray tc.TCGenerics
                    CIHintPath = failwith "TODO"
                }
            v.YieldFact(IsContract(t, info))
            for m in decl.InterfaceBody do
                v.TypeMember(tc, t, m)

        member v.InterfaceDeclaration(ctx, decl: S.InterfaceDeclaration) =
            v.InterfaceDeclaration(ctx, true, decl)

        member v.Parameter(ctx, par) =
            match par with
            | S.P1 (name, t) ->
                let sub = { ctx with TCHint = Some name }
                let ty = v.Type(ctx, t)
                NamedParameter (name, ty)
            | S.P2 (name, v) ->
                FixedParameter (name, v)

        member v.Signatures(ctx, gs, ps, ret) =
            let (req, opt, rest) =
                match ps with
                | S.Ps1 req -> (req, [], None)
                | S.Ps2 (req, opt) -> (req, opt, None)
                | S.Ps3 (req, opt, rest) -> (req, opt, Some rest)
            let conv x = v.Parameter(ctx, x)
            let req = List.map conv req
            let opt = List.map conv opt
            let rest = Option.map conv rest
            let ret =
                match ret with
                | S.TVoid -> None
                | t -> Some (v.Type(ctx, t))
            [
                for opt in List.inits opt ->
                    {
                        CSMethodGenerics = gs
                        CSParameters = req @ opt
                        CSRestParameter = rest
                        CSReturns = ret
                    }
            ]

        member v.Signatures(ctx: TypeContext, c, call, isNew: bool) =
            let (S.CS (generics, ps, ret)) = call
            let sub = { ctx with TCGenericsM = v.TypeParameters(generics) }
            for s in v.Signatures(sub, List.ofArray sub.TCGenericsM, ps, ret) do
                if isNew then CanConstruct (c, s) else CanCall (c, s)
                |> v.YieldFact

        member v.SourceFile(sf: D.SourceFile) =
            // TODO: recognize if this is an external module or a normal file?
            // For now, only deal with normal files.
            let (S.DSF decls) = sf.Syntax
            let globalScope = Scope GlobalModule
            let mctx : ModuleContext =
                {
                    ParentModule = GlobalModule // different for external-modules
                    PrivateScope = globalScope
                    ScopeChain = ScopeChain.InitialScope globalScope
                }
            let tctx = TypeContext.Create(mctx)
            for decl in decls do
                match decl with
                | S.DE1 _ -> () // export assignments not possible in normal source files
                | S.DE2 (_, decl) -> // no export modifier in normal source files
                    v.InterfaceDeclaration(mctx, decl)
                | S.DE3 (exp, imp) ->
                    // Quote: "Except for ImportDeclarations, AmbientModuleElements
                    // always declare exported entities regardless of
                    // whether they include the optional export modifier"
                    // So looks like export modifier matters here.
                    v.ImportDecalration(mctx, exp, imp)
                | S.DE4 _ -> () // TODO: external imports are not yet suported
                | S.DE5 (_, decl) -> // no export modifier in normal source files
                    match decl with
                    | S.AD1 d -> v.AmbientVariableDeclaration(mctx, tctx, d)
                    | S.AD2 (S.AFD (id, s)) -> v.AmbientVariableDeclaration(mctx, tctx, id, S.TObject [S.TM2 s])
                    | S.AD3 c -> v.AmbientClassDeclaration(mctx, tctx, c)
                    | S.AD4 e -> v.AmbientEnumDeclaration(mctx, tctx, e)
                    | S.AD5 m -> v.AmbientModuleDeclaration(mctx, m)
                    | S.AD6 _ -> () // TODO: ambient external modules

        member v.Type(ctx, t, ?objectContract) =
            match t with
            | S.TAny -> TAny
            | S.TNumber -> TNumber
            | S.TBoolean -> TBoolean
            | S.TGeneric n -> TGeneric n
            | S.TGenericM m -> TGenericM m
            | S.TString -> TString
            | S.TVoid -> TAny
            | S.TReference (S.TRef (tN, gs)) -> v.TypeReference(ctx, tN, gs)
            | S.TQuery tQ -> TOf (ctx.TCScopeChain, tQ)
            | S.TArray t -> TArray (v.Type(ctx, t))
            | S.TObject [] -> TAny
            | S.TObject ms ->
                let c =
                    match objectContract with
                    | None -> AnonymousContract (Box t)
                    | Some c -> c
                v.TypeObject(ctx, ms, c)

        member v.TypeMember(ctx: TypeContext, c: Contract, tm: S.TypeMember) =
            match tm with
            | S.TM1 prop ->
                let (name, ty, opt) =
                    match prop with
                    | S.OptProp (name, ty) -> (name, ty, true)
                    | S.Prop (name, ty) -> (name, ty, false)
                let ctx = { ctx with TCHint = Some name }
                let ty = v.Type(ctx, ty, objectContract = PropertyContract (c, name))
                HasProperty (c, name, ty, opt)
                |> v.YieldFact
            | S.TM2 callSig ->
                v.Signatures(ctx, c, callSig, isNew = false)
            | S.TM3 newSig ->
                v.Signatures(ctx, c, newSig, isNew = true)
            | S.TM4 indexSig ->
                match indexSig with
                | S.ByNumber (id, ty) ->
                    CanIndexByNumber (c, id, v.Type(ctx, ty))
                | S.ByString (id, ty) ->
                    CanIndexByString (c, id, v.Type(ctx, ty))
                |> v.YieldFact

        member v.TypeObject(ctx, ms: list<_>, c) =
            let info =
                {
                    CIExtends = []
                    CIFlags = ContractFlags.IsAnonymous
                    CIGenerics = []
                    CIHintPath = failwith "TODO"
                }
            v.YieldFact(IsContract(c, info))
            for m in ms do
                v.TypeMember(ctx, c, m)
            TContract (c, [])

        member v.TypeParameters(ps) =
            // TODO: constraints -- currently ignored.
            [|
                for p in ps ->
                    match p with
                    | S.TP1 id | S.TP2 (id, _) -> id
            |]

        member v.TypeReference(ctx, tN, gs) =
            let t x = v.Type(ctx, x)
            let def () = TNamed (ctx.TCScopeChain, tN, List.map t gs)
            match gs, tN with
            | [], S.TN1 t ->
                match Array.IndexOf(ctx.TCGenerics, t) with
                | -1 ->
                    match Array.IndexOf(ctx.TCGenericsM, t) with
                    | -1 -> def ()
                    | i -> TGenericM i
                | i -> TGeneric i
            | _ -> def ()

        member v.YieldFact(fact: Fact) =
            printfn "%A" fact // TODO

