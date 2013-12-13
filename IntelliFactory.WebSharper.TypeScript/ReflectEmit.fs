namespace IntelliFactory.WebSharper.TypeScript

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
module C = Contracts

/// Implements assembly generation via System.Reflection.Emit.
module internal ReflectEmit =

    type CodeContainer =
        abstract Nodes : seq<Name * CodeNode>

    and CodeNode =
        | ContainerNode of CodeContainer
        | ContractNode of C.Contract
        | ValueNode of C.Type

    type Config =
        {
            AssemblyName : string
            CodeContainer : CodeContainer
            TopLevelClassName : string
        }

    type State =
        {
            Config : Config
            ContainerTable : Dictionary<CodeContainer,TypeBuilder>
            ContractTable : Dictionary<C.Contract,TypeBuilder>
            CreatedTypes : ResizeArray<TypeBuilder>
            ModuleBuilder : ModuleBuilder
        }

        static member Create(cfg, mB) =
            {
                Config = cfg
                ContainerTable = Dictionary()
                ContractTable = Dictionary()
                CreatedTypes = ResizeArray()
                ModuleBuilder = mB
            }

    let SealedPubClass =
        TypeAttributes.Class
        ||| TypeAttributes.Public
        ||| TypeAttributes.Sealed

    let PublicInterface =
        TypeAttributes.Interface
        ||| TypeAttributes.Public

    [<Sealed>]
    type Pass1(st) =

        member p.CodeNode(parent, name, node) =
            match node with
            | ContainerNode c -> p.NestedContainer(parent, name, c)
            | ContractNode c -> p.ContractNode(parent, name, c)
            | ValueNode _ -> ()

        member p.CodeNodes(parent, c: CodeContainer) =
            for (name, node) in c.Nodes do
                p.CodeNode(parent, name, node)

        member p.ContractNode(parent: TypeBuilder, name: Name, c: C.Contract) =
            let tB = parent.DefineNestedType(name.Text, PublicInterface)
            st.ContractTable.[c] <- tB
            st.CreatedTypes.Add(tB)
            ()

        member p.NestedContainer(parent: TypeBuilder, name: Name, c) =
            let tB = parent.DefineNestedType(name.Text, SealedPubClass)
            st.CreatedTypes.Add(tB)
            p.CodeNodes(tB, c)

        member p.TopLevelContainer(c) =
            let tB = st.ModuleBuilder.DefineType(st.Config.TopLevelClassName, SealedPubClass)
            st.CreatedTypes.Add(tB)
            p.CodeNodes(tB, c)

        static member Do(st) =
            Pass1(st).TopLevelContainer(st.Config.CodeContainer)


//    let buildNestedClass (mb: ModuleBuilder) (parent: TypeBuilder) =
//        parent.DefineNestedType(naeTypeAttributes.Interface
//        
//        mb.DefineType
//        mb.DefineN
//        ()


    let objT = typeof<obj>
    let boolT = typeof<bool>
    let numberT = typeof<int> // TODO: WebSharper.Number?
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

    let NotImplementedConstructor =
        typeof<NotImplementedException>.GetConstructor(Array.empty)

    let memo f =
        Memoization.Memoize Memoization.Options.Default f

    [<Sealed>]
    type Pass2(st: State) =

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

        member b.Container(c: CodeContainer) =
            let tB = st.ContainerTable.[c]
            for (name, node) in c.Nodes do
                match node with
                | ContainerNode c -> b.Container(c)
                | ContractNode c -> b.Contract(c)
                | ValueNode ty -> b.Value(tB, name, ty)

        member b.Contract(c: C.Contract) =
            let tB = st.ContractTable.[c]
            let ctx = Unchecked.defaultof<_>
            for KeyValue (propName, prop) in c.Properties do
                match prop.Value with
                /// TODO: methods, func.
                | ty -> b.Property(ctx, tB, propName, ty)

        member b.NotImplemented(m: MethodBuilder) =
            let gen = m.GetILGenerator()
            gen.Emit(OpCodes.Newobj, NotImplementedConstructor)
            gen.Emit(OpCodes.Throw)
            gen.Emit(OpCodes.Ret)

        member b.Property(ctx, tB: TypeBuilder, name: Name, ty) =
            /// if method-like, scroll through overloads.
            let ty = b.Type(ctx, ty)
            let prop = tB.DefineProperty(name.Text, PropertyAttributes.None, ty, Array.empty)
            ()

        member b.StaticProperty(tB: TypeBuilder, name: Name, ty) =
            let ty = b.Type(DefaultContext, ty)
            let n = name.Text
            let prop = tB.DefineProperty(n, PropertyAttributes.None, ty, Array.empty)
            let attrs = MethodAttributes.Public ||| MethodAttributes.Static
            let gm = tB.DefineMethod("get_" + n, attrs, ty, Array.empty)
            let sm = tB.DefineMethod("set_" + n, attrs, voidT, [| ty |])
            prop.SetGetMethod(gm)
            prop.SetSetMethod(sm)
            b.NotImplemented(gm)
            b.NotImplemented(sm)


//        member this.BuildProperty(tb: TypeBuilder, id: NetId, ty: Type, par: seq<Type>, kind: Kind) : unit =
//            let attrs = kind.MethodAttributes
//            let ty = this.BuildType(ty)
//            let par = Array.map this.BuildType (Seq.toArray par)
//            let gm = tb.DefineMethod("get_" + id.Name, attrs, ty, par)
//            let sm = tb.DefineMethod("set_" + id.Name, attrs, typeof<Void>, Array.append par [| ty |])
//            prop.SetGetMethod(gm)
//            prop.SetSetMethod(sm)
//            match kind with
//            | Instance ItemByNumber
//            | Instance ItemByString ->
//                let attrs = this.BuildAttribute(kind)
//                for a in attrs do
//                    gm.SetCustomAttribute(a)
//                    sm.SetCustomAttribute(a)
//            |  _ ->
//                this.BuildAttribute(kind)
//                |> List.iter prop.SetCustomAttribute
//            MakeNotImplemented gm
//            MakeNotImplemented sm

        member b.Type(ctx, ty) : Type =
            let inline ( ! ) t = b.Type(ctx, t)
            match ty with
            | C.TAny -> objT
            | C.TArray x -> arrayType.[!x]
            | C.TBoolean -> boolT
            | C.TGeneric k -> ctx.Generics.[k]
            | C.TGenericM k -> ctx.GenericsM.[k]
            | C.TLazy v -> !v.Value
            | C.TNamed (c, []) ->
                st.ContractTable.[c] :> Type
            | C.TNamed (c, xs) ->
                let xs = Array.map (!) (Array.ofList xs)
                genInst.[(st.ContractTable.[c] :> Type, xs)]
            | C.TNumber -> numberT
            | C.TString -> stringT

        member b.Value(tB: TypeBuilder, name: Name, ty: C.Type) =
            /// if a method-like value, define a static m. for each signature overload
            /// if on the other hand not a static method like, then define a static property
            b.StaticProperty(tB, name, ty)
            ()

        static member Do(st: State) =
            Pass2(st).Container(st.Config.CodeContainer)

    module Pass3 =

        let Do st =
            for ty in st.CreatedTypes do
                ty.CreateType() |> ignore

    // TODO: does DefineDynamicAssembly leak any resources similar to Assembly.Load?
    let Construct cfg =
        let name = AssemblyName(cfg.AssemblyName)
        let n = name.Name
        let dom = AppDomain.CurrentDomain
        let folder = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
        Directory.CreateDirectory(folder) |> ignore
        try
            let aB = dom.DefineDynamicAssembly(name, AssemblyBuilderAccess.Save, folder)
            let mB = aB.DefineDynamicModule(name = n, fileName = n, emitSymbolInfo = false)
            let st = State.Create(cfg, mB)
            Pass1.Do st
            Pass2.Do st
            Pass3.Do st
            aB.Save(n  + ".dll")
            File.ReadAllBytes(Path.Combine(folder, n + ".dll"))
        finally
            Directory.Delete(folder, true)

///// Realizes the assembly definitions using System.Reflection.Emit.
//module private Compilation =
//
//    [<AutoOpen>]
//    module private Util =
//
//        type Kind =
//            | Instance of Key
//            | Static of S.Name
//
//            member this.MethodAttributes =
//                match this with
//                | Instance _ -> MethodAttributes.Public
//                | Static _ -> MethodAttributes.Public ||| MethodAttributes.Static
//

//

//
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
//
//    [<Sealed>]
//    type private Builder(na: NamedAssembly, typesTable: M.Table<NetName,TypeBuilder>) =
//
//        member this.BuildAttribute(kind: Kind) : list<CustomAttributeBuilder> =
//            match kind with
//            | Kind.Instance key ->
//                match key with
//                | Call -> [CallBuilder]
//                | ItemByNumber -> [ItemBuilder]
//                | ItemByString -> [ItemBuilder]
//                | New -> [NewBuilder]
//                | Property id -> MakeSingletonAttribute (Choice2Of2 id)
//            | Kind.Static name -> MakeSingletonAttribute (Choice1Of2 name)
//
//        member this.BuildContract(c: Contract, n: NetName, m: Mp.Mapping<Key,NetId>) =
//            let self = typesTable.[n]
//            for (key, prop) in c.Properties.GetPairs() do
//                this.BuildMember(self, Instance key, m.[key], prop)
//
//        member this.BuildMember(tb: TypeBuilder, kind: Kind, id: NetId, m: Member) : unit =
//            match m with
//            | OverloadedMember ov -> this.BuildMethod(tb, id, ov, kind)
//            | TypedMember ty -> this.BuildProperty(tb, id, ty, Seq.empty, kind)
//
//        member this.BuildMethod(tb: TypeBuilder, id: NetId, overloads: Overloads, kind: Kind) : unit =
//            let attrs = kind.MethodAttributes
//            for (ms, ret) in overloads.Variants do
//                let ret =
//                    let t = this.BuildType(ret)
//                    if t = typeof<unit> then typeof<Void> else t
//                let (ps, rest) = Signatures.Split ms
//                let par =
//                    [|
//                        for p in ps do
//                            yield this.BuildType(p)
//                        match rest with
//                        | Some t -> yield this.BuildType(t).MakeArrayType()
//                        | None -> ()
//                    |]
//                let m = tb.DefineMethod(id.Name, attrs, ret, par)
//                match rest with
//                | Some _ -> MakeRest (m.DefineParameter(par.Length - 1, ParameterAttributes.None, "rest"))
//                | None -> ()
//                this.BuildAttribute(kind)
//                |> List.iter m.SetCustomAttribute
//                MakeNotImplemented m
//
//        member this.BuildNamedDefinition(def: NamedDefinition) =
//            match def with
//            | NamedContract (x, y, z) -> this.BuildContract(x, y, z)
//            | NamedSingleton (s, n) ->
//                let parent = typesTable.[n.Parent.Value]
//                this.BuildMember(parent, Static s.SyntaxName, n.Local, s.Member)
//

//



//    let Compile (na: NamedAssembly) (mb: ModuleBuilder) : unit =
//        let isOpenType =
//            let defs =
//                na.Definitions
//                |> Seq.choose (function
//                    | NamedContract (contract, name, mapping) ->
//                        if contract.AllowsConstructor then Some name else None
//                    | _ -> None)
//            HashSet(defs).Contains
//        let typeTable =
//            let buildType (buildType: NetName -> TypeBuilder) (name: NetName) : TypeBuilder =
//                let builder =
//                    if name = na.EntryPoint then
//                        mb.DefineType(name.Text, RegularClass)
//                    else
//                        let parent = buildType name.Parent.Value
//                        parent.DefineNestedType(name.Local.Name, NestedClass)
//                if not (isOpenType name) then
//                    builder.DefineDefaultConstructor(MethodAttributes.Private)
//                    |> ignore
//                else
//                    let ctor = builder.DefineDefaultConstructor(MethodAttributes.Public)
//                    ctor.SetCustomAttribute(DefaultConstructorAttribute)
//                builder
//            M.MemoizeRecursive (M.Options()) buildType
//        let builder = Builder(na, typeTable)
//        for def in na.Definitions do
//            builder.BuildNamedDefinition(def)
//        for key in typeTable.GetKeys() do
//            typeTable.[key].CreateType()
//            |> ignore
