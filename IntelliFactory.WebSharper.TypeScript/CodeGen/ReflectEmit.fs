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

open System.Reflection
open System.Reflection.Emit

module N = Naming
module S = Shapes

/// Implements assembly generation via System.Reflection.Emit.
module internal ReflectEmit =

    type Config =
        {
            AssemblyName : string
            TopLevelClassName : string
            TopModule : N.TopModule
        }

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
            ModuleBuilder : ModuleBuilder
        }

        static member Create(cfg, mB) =
            {
                Config = cfg
                ContainerTable = ModuleTable()
                ContractTable = Dictionary()
                CreatedTypes = ResizeArray()
                ModuleBuilder = mB
            }

    module Attr =

        let DefaultCtor =
            MethodAttributes.Private
            ||| MethodAttributes.SpecialName
            ||| MethodAttributes.RTSpecialName

        let Interface =
            TypeAttributes.Abstract
            ||| TypeAttributes.Interface
            ||| TypeAttributes.NestedPublic

        let Module =
            TypeAttributes.Class
            ||| TypeAttributes.NestedPublic
            ||| TypeAttributes.Sealed

        let TopLevelModule =
            TypeAttributes.Abstract
            ||| TypeAttributes.Class
            ||| TypeAttributes.Public

    let NotImplementedConstructor =
        typeof<NotImplementedException>.GetConstructor(Array.empty)

    type MethodBuilder with

        member this.NotImplemented() =
            let gen = this.GetILGenerator()
            gen.Emit(OpCodes.Newobj, NotImplementedConstructor)
            gen.Emit(OpCodes.Throw)
            gen.Emit(OpCodes.Ret)

    [<Sealed>]
    type Pass1(st) =

        member p.Contract(parent: TypeBuilder, c: N.Contract) =
            if c.IsReified then
                let name = c.Name
                let tB = parent.DefineNestedType(name.Text, Attr.Interface)
                st.ContractTable.Add(c, tB)
                st.CreatedTypes.Add(tB)
                let gs = [| for n in c.Generics -> n.Text |]
                if gs.Length > 0 then
                    tB.DefineGenericParameters()
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
    let numberT = typeof<int> // TODO: WebSharper.Number?
    let stringT = typeof<string>
    let funTD = typedefof<_->_>
    let unitT = typeof<unit>
    let voidT = typeof<Void>

    let ParamArrayAttributeConstructor =
        typeof<ParamArrayAttribute>.GetConstructor([||])

    let paramArray () =
        CustomAttributeBuilder(ParamArrayAttributeConstructor, [||])

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
        | InterfaceMethod
        | NewMethod
        | StaticMethod

    let methodAttributes k =
        match k with
        | CallMethod
        | InterfaceMethod
        | NewMethod ->
            MethodAttributes.Abstract
            ||| MethodAttributes.HideBySig
            ||| MethodAttributes.PrivateScope
            ||| MethodAttributes.Public
            ||| MethodAttributes.Virtual
        | StaticMethod ->
            MethodAttributes.PrivateScope
            ||| MethodAttributes.Public
            ||| MethodAttributes.Static

    type PropertyKind =
        | InterfaceProperty
        | StaticProperty

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
            if c.IsReified then
                let tB = st.ContractTable.[c]
                let ctx = { DefaultContext with Generics = tB.GenericTypeParameters }
                for ty in c.Extends do
                    tB.AddInterfaceImplementation(b.Type(ctx, ty))
                match c.ByNumber with
                | None -> ()
                | Some i -> b.Indexer(ctx, tB, i.IndexerName, numberT, i.IndexerType)
                match c.ByString with
                | None -> ()
                | Some i -> b.Indexer(ctx, tB, i.IndexerName, stringT, i.IndexerType)
                if Seq.isEmpty c.Call |> not then
                    b.Signatures(CallMethod, ctx, tB, "Call", c.Call)
                if Seq.isEmpty c.New |> not then
                    b.Signatures(NewMethod, ctx, tB, "New", c.New)
                for prop in c.Properties do
                    match prop.Type with
                    | N.MethodType ss -> b.Signatures(InterfaceMethod, ctx, tB, prop.Id.Text, ss)
                    | ty -> b.Property(InterfaceProperty, ctx, tB, prop.Id.Text, prop.Type)

        member b.Indexer(ctx, tB: TypeBuilder, paramName: N.Id, paramType, retType) =
            let pA = PropertyAttributes.None
            let mA =
                MethodAttributes.Abstract
                ||| MethodAttributes.HideBySig
                ||| MethodAttributes.PrivateScope
                ||| MethodAttributes.Public
                ||| MethodAttributes.SpecialName
                ||| MethodAttributes.Virtual
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
            /// TODO: custom attributes here.

        member b.ParamArray(p: ParameterBuilder) =
            p.SetCustomAttribute(paramArray())

        member b.Property(pK, ctx, tB: TypeBuilder, name: string, ty: N.Type) =
            let pA = PropertyAttributes.None
            let pT = b.Type(ctx, ty)
            let pB = tB.DefineProperty(name, pA, pT, Array.empty)
            let mA =
                MethodAttributes.SpecialName |||
                match pK with
                | InterfaceProperty -> methodAttributes InterfaceMethod
                | StaticProperty -> methodAttributes StaticMethod
            let gM = tB.DefineMethod("get_" + name, mA, pT, Array.empty)
            let sM = tB.DefineMethod("set_" + name, mA, voidT, [| pT |])
            sM.DefineParameter(1, ParameterAttributes.None, "value") |> ignore
            pB.SetGetMethod(gM)
            pB.SetSetMethod(sM)
            match pK with
            | InterfaceProperty -> ()
            | StaticProperty ->
                gM.NotImplemented()
                sM.NotImplemented()

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

        member b.Signature(mK, ctx0, tB: TypeBuilder, methodName: string, s: N.Signature) =
            let mA = methodAttributes mK
            let mB = tB.DefineMethod(methodName, mA)
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
                |> b.ParamArray
            | _ -> ()
            match mK with
            | InterfaceMethod | CallMethod | NewMethod -> ()
            | StaticMethod -> mB.NotImplemented()
            /// TODO: this needs attribute annotations.

        member b.Signatures(mK, ctx, tB, methodName: string, ss) =
            let ss =
                ss
                |> Seq.distinctBy (fun s -> b.SignatureIdentity(ctx, s))
                |> Seq.toArray
            for s in ss do
                b.Signature(mK, ctx, tB, methodName, s)

        member b.Type(ctx, ty) : Type =
            let inline ( ! ) t = b.Type(ctx, t)
            match ty with
            | N.TAny -> objT
            | N.TArray x -> arrayType.[!x]
            | N.TBoolean -> boolT
            | N.TGeneric k -> ctx.Generics.[k]
            | N.TGenericM k -> try ctx.GenericsM.[k] with e -> failwithf "GenericsM? %i when avail %i" k ctx.GenericsM.Length
            | N.TNamed (c, xs) ->
                match c.Kind with
                | S.FunctionContract (dom, range) ->
                    let dom = Array.map (!) (Array.ofList dom)
                    let range =
                        match range with
                        | None -> unitT
                        | Some t -> !t
                    funType.[(dom, range)]
                | _ ->
                    match xs with
                    | [] -> st.ContractTable.[c] :> Type
                    | _ ->
                        let xs = Array.map (!) (Array.ofList xs)
                        genInst.[(st.ContractTable.[c] :> Type, xs)]
            | N.TNumber -> numberT
            | N.TString -> stringT

        member b.Value(tB, v: N.Value) =
            let n = v.Id
            match v.Type with
            | N.MethodType ss -> b.Signatures(StaticMethod, DefaultContext, tB, v.Id.Text, ss)
            | ty -> b.Property(StaticProperty, DefaultContext, tB, v.Id.Text, ty)

        static member Do(st: State) =
            Pass2(st).Container(st.Config.TopModule)

    module Pass3 =

        let Do st =
            for ty in st.CreatedTypes do
                ty.CreateType() |> ignore

    // TODO: does DefineDynamicAssembly leak any resources similar to Assembly.Load?
    let ConstructAssembly cfg =
        let name = AssemblyName(cfg.AssemblyName)
        let n = name.Name
        let fN = n + ".dll"
        let dom = AppDomain.CurrentDomain
        let folder = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
        let fP = Path.Combine(folder, fN)
        Directory.CreateDirectory(folder) |> ignore
        try
            let aB = dom.DefineDynamicAssembly(name, AssemblyBuilderAccess.Save, folder)
            let mB = aB.DefineDynamicModule(name = n, fileName = fN, emitSymbolInfo = false)
            let st = State.Create(cfg, mB)
            Pass1.Do st
            Pass2.Do st
            Pass3.Do st
            aB.Save(fN)
            File.ReadAllBytes(fP)
        finally
            Directory.Delete(folder, true)

//        let RestAttributeBuilder : CustomAttributeBuilder =
//            CustomAttributeBuilder(typeof<ParamArrayAttribute>.GetConstructor(Array.empty), Array.empty)
//
//        let MakeRest (p: ParameterBuilder) =
//            p.SetCustomAttribute(RestAttributeBuilder)
//
//        let RegularClass =
//            TypeAttributes.Class
//            ||| TypeAttributes.Public
//            ||| TypeAttributes.Sealed
//
//        let NestedClass =
//            TypeAttributes.Class
//            ||| TypeAttributes.NestedPublic
//            ||| TypeAttributes.Sealed
//
//        let MakeMacroAttribute : System.Type -> CustomAttributeBuilder =
//            let macroCtor = typeof<A.MacroAttribute>.GetConstructor([| typeof<System.Type> |])
//            fun t -> CustomAttributeBuilder(macroCtor, [| box t |])
//
//        let CallBuilder = MakeMacroAttribute typeof<WebSharper.Macros.CallMacro>
//        let NewBuilder = MakeMacroAttribute typeof<WebSharper.Macros.NewMacro>
//        let ItemBuilder = MakeMacroAttribute typeof<WebSharper.Macros.ItemMacro>
//
//        let DefaultConstructorAttribute =
//            let t = typeof<A.InlineAttribute>.GetConstructor([| typeof<string> |])
//            CustomAttributeBuilder(t, [| box "{}" |])
//
//        let MakeSingletonAttribute : Choice<S.Name,S.Identifier> -> list<CustomAttributeBuilder> =
//            let nameCtor1 = typeof<A.NameAttribute>.GetConstructor([| typeof<string> |])
//            let nameCtorN = typeof<A.NameAttribute>.GetConstructor([| typeof<string[]> |])
//            let stubCtor = typeof<A.StubAttribute>.GetConstructor(Array.empty)
//            let stubBuilder = CustomAttributeBuilder(stubCtor, Array.empty)
//            fun name ->
//                let arg =
//                    match name with
//                    | Choice1Of2 name ->
//                        name.List
//                        |> List.toArray
//                        |> Array.map (fun x -> x.Name)
//                        |> box
//                    | Choice2Of2 id ->
//                        box id.Name
//                let nameCtor =
//                    match name with
//                    | Choice1Of2 _ -> nameCtorN
//                    | Choice2Of2 _ -> nameCtor1
//                [
//                    CustomAttributeBuilder(nameCtor, [| arg |])
//                    stubBuilder
//                ]

