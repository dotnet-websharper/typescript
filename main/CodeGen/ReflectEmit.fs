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

module N = Naming
module S = Shapes

/// Implements assembly generation via System.Reflection.Emit.
module internal ReflectEmit =

    type System.Reflection.Emit.TypeBuilder with

        member self.GenericTypeParameters =
            if self.ContainsGenericParameters then
                self.GetGenericArguments()
            else
                Array.empty

        member self.ImplementedInterfaces =
            self.GetInterfaces()

    type Config =
        {
            AssemblyName : string
            TemporaryFolder : string
            TopLevelClassName : string
            TopModule : N.TopModule
        }

    type MethodView =
        | IsConstructor of seq<N.Signature>
        | IsMethod of seq<N.Signature>
        | IsNormal

    /// A test for properites that may compile to CLR methods.
    let GetMethodView (declaring: option<N.Contract>) (t: N.Type) : MethodView =
        let genericArity =
            match declaring with
            | None -> 0
            | Some c -> Seq.length c.Generics
        let properGenerics gs =
            let rec proper i gs =
                match gs with
                | [] -> genericArity = i
                | N.TGeneric j :: gs when i = j -> proper (i + 1) gs
                | _ -> false
            proper 0 gs
        let isProper (c: N.Contract) (gs: list<N.Type>) =
            c.IsAnonymous
            && properGenerics gs
            && Seq.length c.Generics = genericArity
            && c.ByNumber.IsNone
            && c.ByString.IsNone
            && Seq.isEmpty c.Extends
            && Seq.isEmpty c.Properties
        let isMethod (c: N.Contract) (gs: list<N.Type>) =
            Seq.isEmpty c.New
            && not (Seq.isEmpty c.Call)
        let isConstructor (c: N.Contract) (gs: list<N.Type>) =
            Seq.isEmpty c.Call
            && not (Seq.isEmpty c.New)
        match t with
        | N.TNamed (c, gs) when isProper c gs ->
            if isMethod c gs then
                IsMethod c.Call
            elif isConstructor c gs then
                IsConstructor c.New
            else
                IsNormal
        | _ ->
            IsNormal

    type FuncView =
        {
            FuncArgs : list<N.Type>
            FuncRet : option<N.Type>
        }

    let SubstituteTypeGenerics (t: N.Type) (subst: list<N.Type>) =
        match subst with
        | [] -> t
        | _ ->
            let subst = List.toArray subst
            let rec s t =
                match t with
                | N.TArray t -> N.TArray (s t)
                | N.TCompiled (t, ts) -> N.TCompiled (t, List.map s ts)
                | N.TGeneric i -> subst.[i]
                | N.TNamed (c, ts) -> N.TNamed (c, List.map s ts)
                | N.TAny
                | N.TBoolean
                | N.TGenericM _
                | N.TNumber
                | N.TString -> t
            s t

    let (|FuncType|_|) (t: N.Type) : option<FuncView> =
        match t with
        | N.TNamed (c, args) ->
            let ok =
                c.IsAnonymous
                && c.ByNumber.IsNone
                && c.ByString.IsNone
                && Seq.isEmpty c.Extends
                && Seq.isEmpty c.New
                && Seq.isEmpty c.Properties
            if ok then
                match Seq.toList c.Call with
                | [s] ->
                    let isSimpleParam p =
                        match p with
                        | N.Parameter.Param _ -> true
                        | _ -> false
                    let getParamType p =
                        match p with
                        | N.Parameter.Param (_, t) -> t
                        | _ -> N.TAny
                    let ok =
                        s.MethodGenerics.IsEmpty
                        && s.RestParameter.IsNone
                        && List.forall isSimpleParam s.Parameters
                    if ok then
                        let subst t = SubstituteTypeGenerics t args
                        let ps =
                            s.Parameters
                            |> List.map (getParamType >> subst)
                        Some {
                            FuncArgs = ps
                            FuncRet = Option.map subst s.ReturnType
                        }
                    else None
                | _ -> None
            else None
         | _ -> None

    type RecordInfo =
        {
            OptionalProperties : seq<N.Property>
            RequiredProperties : seq<N.Property>
        }

    type TypeView =
        | IsGeneralType
        | IsRecord of RecordInfo

    let IsPropertyOfSimpleType (c: N.Contract) (p: N.Property) =
        let t = p.Type
        match t with
        | N.TAny
        | N.TArray _
        | N.TBoolean
        | N.TCompiled _
        | N.TGeneric _
        | N.TGenericM _
        | N.TNumber
        | N.TString -> true
        | N.TNamed _ ->
            match t with
            | FuncType _ when p.Optional -> true
            | _ ->
                match GetMethodView (Some c) t with
                | IsNormal -> true
                | IsConstructor _ | IsMethod _ -> false

    let GetTypeView (c: N.Contract) : TypeView =
        let isRecord =
            c.ByNumber.IsNone
            && c.ByString.IsNone
            && Seq.isEmpty c.Call
            && Seq.isEmpty c.New
            && Seq.isEmpty c.Extends
            && Seq.forall (IsPropertyOfSimpleType c) c.Properties
        if isRecord then
            let optP = ResizeArray()
            let reqP = ResizeArray()
            for p in c.Properties do
                if p.Optional then
                    optP.Add(p)
                else
                    reqP.Add(p)
            IsRecord {
                OptionalProperties = optP.ToArray() :> seq<_>
                RequiredProperties = reqP.ToArray() :> seq<_>
            }
        else
            IsGeneralType

    /// For starters, we try to traverse all contracts to decide which ones
    /// are `reified`, that is, need a CLR representation.
    [<Sealed>]
    type Pass0 private () =
        let reified = HashSet<N.Contract>()

        member private p.Contract(c: N.Contract) =
            Seq.iter p.Type c.Extends
            Option.iter p.Indexer c.ByNumber
            Option.iter p.Indexer c.ByString
            Seq.iter p.Signature c.Call
            Seq.iter p.Signature c.New
            for prop in c.Properties do
                p.Property(c, prop)

        member private p.Indexer(i: N.Indexer) =
            p.Type i.IndexerType

        member private p.IsReified(c: N.Contract) =
            not c.IsAnonymous || reified.Contains c

        member private p.Mark(c: N.Contract) =
            if c.IsAnonymous then
                reified.Add(c) |> ignore

        member private p.Module<'T>(m: N.Module<'T>) : unit =
            for sM in m.Modules do
                p.Module(sM)
            for c in m.Contracts do
                p.Contract(c)
            for v in m.Values do
                p.Value(v)

        member private p.Parameter(par: N.Parameter) =
            match par with
            | N.Parameter.Param (_, ty) -> p.Type ty
            | _ -> ()

        member private p.Property(c: N.Contract, prop: N.Property) =
            match GetMethodView (Some c) prop.Type with
            | IsMethod ss -> Seq.iter p.Signature ss
            | _ -> p.Type prop.Type

        member private p.Signature(s: N.Signature) =
            List.iter p.Parameter s.Parameters
            Option.iter p.Parameter s.RestParameter
            Option.iter p.Type s.ReturnType

        member private p.Type(t: N.Type) =
            match t with
            | N.TAny | N.TBoolean | N.TGeneric _ | N.TGenericM _ | N.TNumber | N.TString -> ()
            | N.TArray t -> p.Type t
            | N.TCompiled (_, ts) -> List.iter p.Type ts
            | N.TNamed (c, ts) ->
                match t with
                | FuncType view ->
                    List.iter p.Type view.FuncArgs
                    Option.iter p.Type view.FuncRet
                | _ ->
                    p.Mark(c)
                    List.iter p.Type ts

        member private p.Value(v: N.Value) =
            match GetMethodView None v.Type with
            | IsNormal -> p.Type v.Type
            | IsMethod ss | IsConstructor ss -> Seq.iter p.Signature ss

        static member Do(cfg) : (N.Contract -> bool) =
            let p = Pass0()
            p.Module(cfg.TopModule)
            p.IsReified

    [<Sealed>]
    type ModuleTable<'T>() =
        let d = Dictionary<obj,'T>()

        member mt.Add(m: N.Module<'X>, v: 'T) = d.Add(m, v)
        member mt.Get<'X>(key: N.Module<'X>) = d.[key]

    type State =
        {
            Config : Config
            ContainerTable : ModuleTable<TypeBuilder>
            ContractTable : Dictionary<N.Contract,TypeBuilder>
            CreatedTypes : ResizeArray<TypeBuilder>
            IsReified : N.Contract -> bool
            ModuleBuilder : ModuleBuilder
        }

        static member Create(cfg, mB) =
            {
                Config = cfg
                ContainerTable = ModuleTable()
                ContractTable = Dictionary()
                CreatedTypes = ResizeArray()
                IsReified = Pass0.Do cfg
                ModuleBuilder = mB
            }

    module Attr =

        let DefaultCtor =
            MethodAttributes.Private
            ||| MethodAttributes.SpecialName
            ||| MethodAttributes.RTSpecialName

        let DefaultPublicCtor =
            MethodAttributes.Public
            ||| MethodAttributes.SpecialName
            ||| MethodAttributes.RTSpecialName

        let Indexer =
            MethodAttributes.Abstract
            ||| MethodAttributes.HideBySig
            ||| MethodAttributes.PrivateScope
            ||| MethodAttributes.Public
            ||| MethodAttributes.SpecialName
            ||| MethodAttributes.Virtual

        let SealedClass =
            TypeAttributes.Class
            ||| TypeAttributes.NestedPublic
            ||| TypeAttributes.Sealed

        let ConfigObjectMethod =
            MethodAttributes.HideBySig
            ||| MethodAttributes.PrivateScope
            ||| MethodAttributes.Public

        let Interface =
            TypeAttributes.Abstract
            ||| TypeAttributes.Interface
            ||| TypeAttributes.NestedPublic

        let InterfaceMethod =
            MethodAttributes.Abstract
            ||| MethodAttributes.HideBySig
            ||| MethodAttributes.PrivateScope
            ||| MethodAttributes.Public
            ||| MethodAttributes.Virtual

        let Module =
            TypeAttributes.Class
            ||| TypeAttributes.NestedPublic
            ||| TypeAttributes.Sealed

        let RecordConstructor =
            MethodAttributes.PrivateScope
            ||| MethodAttributes.Public
            ||| MethodAttributes.SpecialName
            ||| MethodAttributes.RTSpecialName

        let StaticMethod =
            MethodAttributes.PrivateScope
            ||| MethodAttributes.Public
            ||| MethodAttributes.Static

        let TopLevelModule =
            TypeAttributes.Abstract
            ||| TypeAttributes.Class
            ||| TypeAttributes.Public

    let NotImplementedConstructor =
        typeof<NotImplementedException>.GetConstructor(Array.empty)

    type System.Reflection.Emit.ConstructorBuilder with

        member this.NotImplemented() =
            let gen = this.GetILGenerator()
            gen.Emit(OpCodes.Newobj, NotImplementedConstructor)
            gen.Emit(OpCodes.Throw)

    type System.Reflection.Emit.MethodBuilder with

        member this.NotImplemented() =
            let gen = this.GetILGenerator()
            gen.Emit(OpCodes.Newobj, NotImplementedConstructor)
            gen.Emit(OpCodes.Throw)

    [<Sealed>]
    type Pass1(st) =

        member p.Contract(parent: TypeBuilder, c: N.Contract) =
            if st.IsReified c then
                let name = c.Name
                let view = GetTypeView c
                let attr =
                    match view with
                    | IsRecord _ -> Attr.SealedClass
                    | IsGeneralType -> Attr.Interface
                let tB = parent.DefineNestedType(name.Text, attr)
                st.ContractTable.Add(c, tB)
                st.CreatedTypes.Add(tB)
                let gs = [| for n in c.Generics -> n.Text |]
                if gs.Length > 0 then
                    tB.DefineGenericParameters(gs)
                    |> ignore

        member p.Module<'T>(tB, m: N.Module<'T>) =
            p.NoConstructor(tB)
            st.ContainerTable.Add(m, tB)
            st.CreatedTypes.Add(tB)
            for m in m.Modules do
                p.NestedModule(tB, m)
            for c in m.Contracts do
                p.Contract(tB, c)

        member p.NestedModule(parent: TypeBuilder, m: N.NestedModule) =
            let tB = parent.DefineNestedType(m.Id.Text, Attr.Module)
            p.Module(tB, m)

        member p.TopModule(c: N.TopModule) =
            let tB = st.ModuleBuilder.DefineType(st.Config.TopLevelClassName, Attr.TopLevelModule)
            p.Module(tB, c)

        member p.NoConstructor(tB: TypeBuilder) =
            tB.DefineDefaultConstructor(Attr.DefaultCtor) |> ignore

        static member Do(st) =
            Pass1(st).TopModule(st.Config.TopModule)

    [<Sealed>] type Z = class end
    [<Sealed>] type S0<'T> = class end
    [<Sealed>] type S1<'T> = class end
    [<Sealed>] type M<'T> = class end

    let zT = typeof<Z>
    let s0T = typedefof<S0<_>>
    let s1T = typedefof<S1<_>>
    let mT = typedefof<M<_>>

    let rec typeDigit (n: int) =
        match n with
        | 0 -> zT
        | _ ->
            let mutable r = 0
            let q = Math.DivRem(n, 2, &r)
            let d = match r with | 0 -> s0T | _ -> s1T
            d.MakeGenericType(typeDigit q)

    let memo f =
        Memoization.Memoize Memoization.Options.Default f

    let objT = typeof<obj>
    let boolT = typeof<bool>
    let numberT = typeof<double>
    let stringT = typeof<string>
    let funTD = typedefof<_->_>
    let unitT = typeof<unit>
    let voidT = typeof<Void>

    type Context =
        {
            Generics : Type []
            GenericsM : Type []
        }

    let DefaultContext =
        {
            Generics = Array.empty
            GenericsM = Array.empty
        }

    type MethodKind =
        | CallMethod
        | Constructor of NamePath // JavaScript name
        | InterfaceMethod of Name // JavaScript name
        | NewMethod
        | StaticMethod of NamePath // JavaScript name

    type PropertyKind =
        | InterfaceProperty of Name // JavaScript name
        | RecordProperty of Name // JavaScript name
        | StaticProperty of NamePath // JavaScript name

    [<Sealed>]
    type Pass2(st: State) =

        let typeGenDummy = memo typeDigit
        let methodGenDummy = memo (fun n -> mT.MakeGenericType(typeDigit n))

        let arrayType = memo <| fun (t: Type) ->
            t.MakeArrayType()

        let tupleTypeDef = memo <| fun (n: int) ->
            Type.GetType(String.Format("System.Tuple`{0}", n), true)

        let tupleType = memo <| fun ts ->
            tupleTypeDef.[Array.length ts].MakeGenericType(ts)

        let funType = memo <| fun (dom, range) ->
            match Array.length dom with
            | 0 -> funTD.MakeGenericType(unitT, range)
            | 1 -> funTD.MakeGenericType(dom.[0], range)
            | ts -> funTD.MakeGenericType(tupleType.[dom], range)

        let genInst = memo <| fun (main: Type, args) ->
            main.MakeGenericType(args)

        member b.Container<'T>(m: N.Module<'T>) : unit =
            let tB = st.ContainerTable.Get(m)
            for mo in m.Modules do
                b.Container<N.Id>(mo)
            for c in m.Contracts do
                b.Contract(c)
            for v in m.Values do
                b.Value(tB, v)

        member b.Contract(c: N.Contract) =
            if st.IsReified c then
                let tB = st.ContractTable.[c]
                let ctx = { DefaultContext with Generics = tB.GenericTypeParameters }
                match GetTypeView c with
                | IsRecord recInfo ->
                    b.RecordConstructor(ctx, tB, recInfo)
                    for prop in Seq.append recInfo.RequiredProperties recInfo.OptionalProperties do
                        b.Property(RecordProperty prop.Name, ctx, tB, prop.Id, prop.Type)
                | IsGeneralType ->
                    for ty in c.Extends do
                        let t = b.Type(ctx, ty)
                        if t.IsInterface then // can be `obj` concealing a failure
                            tB.AddInterfaceImplementation(t)
                    match c.ByNumber with
                    | None -> ()
                    | Some i -> b.Indexer(ctx, tB, i.IndexerName, numberT, i.IndexerType)
                    match c.ByString with
                    | None -> ()
                    | Some i -> b.Indexer(ctx, tB, i.IndexerName, stringT, i.IndexerType)
                    if Seq.isEmpty c.Call |> not then
                        b.Signatures(CallMethod, ctx, tB, N.Id.Call, c.Call)
                    if Seq.isEmpty c.New |> not then
                        b.Signatures(NewMethod, ctx, tB, N.Id.New, c.New)
                    let declaring = Some c
                    for prop in c.Properties do
                        match GetMethodView declaring prop.Type with
                        | IsMethod ss -> b.Signatures(InterfaceMethod prop.Name, ctx, tB, prop.Id, ss)
                        | _ -> b.Property(InterfaceProperty prop.Name, ctx, tB, prop.Id, prop.Type)

        member b.Indexer(ctx, tB: TypeBuilder, paramName: N.Id, paramType, retType) =
            let pA = PropertyAttributes.None
            let mA = Attr.Indexer
            let pT = [| paramType |]
            let rT = b.Type(ctx, retType)
            let pD = tB.DefineProperty("Item", pA, rT, pT)
            let gM = tB.DefineMethod("get_Item", mA, rT, pT)
            gM.DefineParameter(1, ParameterAttributes.None, paramName.Text) |> ignore
            let sM = tB.DefineMethod("set_Item", mA, voidT, [| paramType; rT |])
            sM.DefineParameter(1, ParameterAttributes.None, paramName.Text) |> ignore
            sM.DefineParameter(2, ParameterAttributes.None, "value") |> ignore
            pD.SetGetMethod(gM)
            pD.SetSetMethod(sM)
            pD.SetCustomAttribute(CustomAttr.Item)

        member b.Property(pK: PropertyKind, ctx, tB: TypeBuilder, name: N.Id, ty: N.Type) =
            let pA = PropertyAttributes.None
            let pT = b.Type(ctx, ty)
            let pB = tB.DefineProperty(name.Text, pA, pT, Array.empty)
            let mA =
                MethodAttributes.SpecialName |||
                match pK with
                | InterfaceProperty jname -> Attr.InterfaceMethod
                | RecordProperty jname -> Attr.ConfigObjectMethod
                | StaticProperty jname -> Attr.StaticMethod
            let gM = tB.DefineMethod("get_" + name.Text, mA, pT, Array.empty)
            let sM = tB.DefineMethod("set_" + name.Text, mA, voidT, [| pT |])
            sM.DefineParameter(1, ParameterAttributes.None, "value") |> ignore
            pB.SetGetMethod(gM)
            pB.SetSetMethod(sM)
            match pK with
            | RecordProperty jname ->
                gM.NotImplemented()
                sM.NotImplemented()
                gM.SetCustomAttribute(CustomAttr.PropertyGet jname.Text)
                sM.SetCustomAttribute(CustomAttr.PropertySet jname.Text)
            | InterfaceProperty jname ->
                gM.SetCustomAttribute(CustomAttr.PropertyGet jname.Text)
                sM.SetCustomAttribute(CustomAttr.PropertySet jname.Text)
            | StaticProperty jname ->
                gM.NotImplemented()
                sM.NotImplemented()
                gM.SetCustomAttribute(CustomAttr.StaticPropertyGet jname)
                sM.SetCustomAttribute(CustomAttr.StaticPropertySet jname)

        member b.RecordConstructor(ctx, tB: TypeBuilder, rI: RecordInfo) =
            let ps : Type [] =
                [|
                    for p in rI.RequiredProperties ->
                        b.Type(ctx, p.Type)
                |]
            let ctor =
                tB.DefineConstructor(Attr.RecordConstructor,
                    CallingConventions.Standard ||| CallingConventions.HasThis,
                    ps)
            let psNames = ResizeArray()
            do
                let mutable i = 0
                for prop in rI.RequiredProperties do
                    let name = prop.Id.Text
                    i <- i + 1
                    psNames.Add(prop.Name.Text)
                    ctor.DefineParameter(i, ParameterAttributes.None, name) |> ignore
            ctor.NotImplemented()
            ctor.SetCustomAttribute(CustomAttr.RecordConstructor psNames)

        /// An arbitrary object computed for comparing signatures for
        /// identity in compiled code, to signatures CLR sees as duplicate.
        member b.SignatureIdentity(ctx, s: N.Signature) =
            let context =
                let gs =
                    List.toArray s.MethodGenerics
                    |> Array.mapi (fun i _ -> methodGenDummy.[i])
                { ctx with GenericsM = gs }
            s.Parameters
            |> List.choose (function
                | N.Parameter.Param (_, ty) -> b.Type(context, ty) |> Some
                | _ -> None)

        member b.Signature(mK, ctx0, tB: TypeBuilder, methodName: N.Id, s: N.Signature) =
            let mA =
                match mK with
                | CallMethod -> Attr.InterfaceMethod
                | Constructor _ -> Attr.StaticMethod
                | InterfaceMethod _ -> Attr.InterfaceMethod
                | NewMethod -> Attr.InterfaceMethod
                | StaticMethod _ -> Attr.StaticMethod
            let mB = tB.DefineMethod(methodName.Text, mA)
            let gs =
                let names =
                    List.toArray s.MethodGenerics
                    |> Array.map (fun n -> n.Text)
                if names.Length > 0
                    then mB.DefineGenericParameters(names)
                    else Array.empty
            let ctx = { ctx0 with GenericsM = Array.map (fun x -> x :> Type) gs }
            let pA = ParameterAttributes.None
            let returnType =
                match s.ReturnType with
                | None -> voidT
                | Some ty -> b.Type(ctx, ty)
            let paramTypes =
                [|
                    for p in s.Parameters do
                        match p with
                        | N.Parameter.Param (_, ty) -> yield b.Type(ctx, ty)
                        | _ -> ()
                    match s.RestParameter with
                    | Some (N.Parameter.Param (_, ty)) ->
                        yield b.Type(ctx, ty)
                    | _ -> ()
                |]
            mB.SetParameters(paramTypes)
            mB.SetReturnType(returnType)
            do
                let mutable i = 0
                for p in s.Parameters do
                    match p with
                    | N.Parameter.Param (name, ty) ->
                        i <- i + 1
                        mB.DefineParameter(i, pA, name.Text) |> ignore
                    | _ -> ()
            match s.RestParameter with
            | Some (N.Parameter.Param (name, ty)) ->
                mB.DefineParameter(paramTypes.Length, pA, name.Text)
                    .SetCustomAttribute(CustomAttr.ParamArray)
            | _ -> ()
            match mK with
            | InterfaceMethod jname ->
                if s.RestParameter.IsSome then
                    mB.SetCustomAttribute(CustomAttr.MethodWithParamArray jname.Text paramTypes.Length)
                else
                    mB.SetCustomAttribute(CustomAttr.Method jname.Text paramTypes.Length)
            | CallMethod ->
                mB.SetCustomAttribute(CustomAttr.Call)
            | NewMethod ->
                mB.SetCustomAttribute(CustomAttr.New)
            | Constructor jname ->
                CustomAttr.Constructor jname paramTypes.Length
                |> mB.SetCustomAttribute
                mB.NotImplemented()
            | StaticMethod jname ->
                let arity = paramTypes.Length
                if s.RestParameter.IsSome then
                    CustomAttr.StaticMethodWithParamArray jname arity
                else
                    CustomAttr.StaticMethod jname arity
                |> mB.SetCustomAttribute
                mB.NotImplemented()

        member b.Signatures(mK, ctx, tB, methodName: N.Id, ss) =
            let ss =
                ss
                |> Seq.distinctBy (fun s -> b.SignatureIdentity(ctx, s))
                |> Seq.toArray
            for s in ss do
                b.Signature(mK, ctx, tB, methodName, s)

        member b.Type(ctx, ty) : Type =
            match ty with
            | N.TAny -> objT
            | N.TArray x -> arrayType.[b.Type(ctx, x)]
            | N.TBoolean -> boolT
            | N.TGeneric k -> ctx.Generics.[k]
            | N.TGenericM k -> ctx.GenericsM.[k]
            | N.TNamed (c, xs) ->
                match ty with
                | FuncType { FuncArgs = dom; FuncRet = range } ->
                    let dom = [| for d in dom -> b.Type(ctx, d) |]
                    let range =
                        match range with
                        | None -> unitT
                        | Some t -> b.Type(ctx, t)
                    funType.[(dom, range)]
                | _ ->
                    match xs with
                    | [] -> st.ContractTable.[c] :> Type
                    | _ ->
                        let xs = Array.map (fun x -> b.Type(ctx, x)) (Array.ofList xs)
                        genInst.[(st.ContractTable.[c] :> Type, xs)]
            | N.TCompiled (ty, ts) ->
                match ts with
                | [] -> ty
                | _ ->
                    let ts =
                        List.toArray ts
                        |> Array.map (fun x -> b.Type(ctx, x))
                    ty.MakeGenericType(ts)
            | N.TNumber -> numberT
            | N.TString -> stringT

        member b.Value(tB, v: N.Value) =
            match GetMethodView None v.Type with
            | IsConstructor ss -> b.Signatures(Constructor v.NamePath, DefaultContext, tB, v.Id, ss)
            | IsMethod ss -> b.Signatures(StaticMethod v.NamePath, DefaultContext, tB, v.Id, ss)
            | IsNormal -> b.Property(StaticProperty v.NamePath, DefaultContext, tB, v.Id, v.Type)

        static member Do(st: State) =
            Pass2(st).Container(st.Config.TopModule)

    module Pass3 =

        let Do st =
            // NOTE: need to consider base types first, otherwise `ty.CreateType` fails with an exception.
            let baseTypes (ty: TypeBuilder) : seq<TypeBuilder> =
                let r = ResizeArray()
                let rec visit (t: Type) =
                    if t <> null then
                        if t.IsArray then
                            visit (t.GetElementType())
                        elif t.IsGenericType && not t.IsGenericTypeDefinition then
                            visit (t.GetGenericTypeDefinition())
                            for t in t.GetGenericArguments() do
                                visit t
                        else
                            match t with
                            | :? TypeBuilder as b -> r.Add(b)
                            | _ -> ()
                visit ty.DeclaringType
                for i in ty.ImplementedInterfaces do
                    visit i
                r :> seq<_>
            for ty in TopSort.Intrinsic st.CreatedTypes baseTypes do
                ty.CreateType() |> ignore

    /// Computes the metadata table.
    [<Sealed>]
    type Pass4(st: State) =

        member p4.Container<'T>(m: N.Module<'T>) : seq<NamePath * Type> =
            Seq.append
                (Seq.collect p4.Container m.Modules)
                (Seq.choose p4.Contract m.Contracts)

        member p4.Contract(c: N.Contract) =
            match c.Origin with
            | Some orig when st.IsReified c ->
                match st.ContractTable.TryGetValue(c) with
                | true, ty -> Some (orig, ty :> Type)
                | _ -> None
            | _ -> None

        static member Do(st: State) =
            Pass4(st).Container(st.Config.TopModule)
            |> Metadata.Table.Create

    module WebSharperCompiler =
        open IntelliFactory.Core
        module FE = IntelliFactory.WebSharper.Compiler.FrontEnd

        (* TODO: propagate error messsages better *)

        let CompileAssemblyWithWebSharper (fileName: string) =
            let opts = FE.Options.Default
            let compiler = FE.Prepare opts stdout.WriteLine
            let resolver = AssemblyResolution.AssemblyResolver.Create()
            let loader = FE.Loader.Create resolver stdout.WriteLine
            let assem = loader.LoadFile fileName
            if not (compiler.CompileAndModify assem) then
                failwith "Could not compile the assembly with WebSharper"
            assem.Write None fileName

    // TODO: does DefineDynamicAssembly leak any resources similar to Assembly.Load?
    let ConstructAssembly cfg =
        let name = AssemblyName(cfg.AssemblyName)
        let n = name.Name
        let fN = n + ".dll"
        let dom = AppDomain.CurrentDomain
        let folder = Path.Combine(cfg.TemporaryFolder, Path.GetRandomFileName())
        let fP = Path.Combine(folder, fN)
        Directory.CreateDirectory(folder) |> ignore
        try
            let aB = dom.DefineDynamicAssembly(name, AssemblyBuilderAccess.Save, folder)
            let mB = aB.DefineDynamicModule(name = n, fileName = fN, emitSymbolInfo = false)
            let st = State.Create(cfg, mB)
            Pass1.Do st
            Pass2.Do st
            Pass3.Do st
            let meta = Pass4.Do st
            do  let bytes = meta.Serialize()
                use s = new MemoryStream(bytes, writable = false)
                mB.DefineManifestResource(Metadata.ResourceName, s, ResourceAttributes.Public)
                aB.Save(fN)
            WebSharperCompiler.CompileAssemblyWithWebSharper fP
            File.ReadAllBytes(fP)
        finally
            Directory.Delete(folder, true)
