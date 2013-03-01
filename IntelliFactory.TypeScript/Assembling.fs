/// Constructs .NET assemblies, includes a built-in algorithm for name disambiguation.
module internal IntelliFactory.TypeScript.Assembling

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Text
open System.Text.RegularExpressions
module A = IntelliFactory.WebSharper.Core.Attributes
module M = Memoization
module Mp = Mappings
module S = Syntax

type NetId = Symbols.Symbol<Symbols.NetChecker>
type NetName = Symbols.Name<Symbols.NetChecker>

module private Identifiers =
    let Call = NetId.Create("Call")
    let Item = NetId.Create("Item")
    let New = NetId.Create("New")

    module Syntax =
        let Call = S.Identifier.Create("Call")
        let Item = S.Identifier.Create("Item")
        let New = S.Identifier.Create("New")

type Key =
    | Call
    | ItemByNumber
    | ItemByString
    | New
    | Property of S.Identifier

    member this.Id =
        match this with
        | Call -> Identifiers.Syntax.Call
        | ItemByNumber | ItemByString -> Identifiers.Syntax.Item
        | New -> Identifiers.Syntax.New
        | Property p -> p

    member this.NetId() =
        match this with
        | Call -> Identifiers.Call
        | ItemByNumber | ItemByString -> Identifiers.Item
        | New -> Identifiers.New
        | Property p -> Symbols.ConvertSymbol p

[<Sealed>]
type Contract(def: ContractDefinition) =
    member this.AllowsConstructor = def.AllowsConstructor
    member this.Definition = def
    member this.HintName = def.HintName
    member this.Properties = def.Properties
    member this.Suffix = def.Suffix

and ContractDefinition =
    {
        AllowsConstructor : bool
        HintName : NetName
        Properties : Mp.Mapping<Key,Member>
        Suffix : NetId
    }

and Member =
    | OverloadedMember of Overloads
    | TypedMember of Type

and [<Sealed>] Overloads(orig: list<Signature * Type>) =
    let variants =
        lazy
            orig
            |> Seq.distinctBy fst
            |> Seq.toList

    member this.IsEmpty = orig.IsEmpty
    member this.Variants = variants.Value
    override this.ToString() =
        sprintf "+%A" this.Variants

and Signature =
    Signatures.Signature<Type>

and Type =
    | AnyType
    | ArrayType of Type
    | BooleanType
    | ContractType of Lazy<Contract>
    | FunctionType of list<Type> * Type
    | NumberType
    | StringType
    | UnitType

[<Sealed>]
type Singleton(hintName: NetName, synName: S.Name, singleton: Member, suffix: NetId) =
    member this.HintName = hintName
    member this.Member = singleton
    member this.Suffix = suffix
    member this.SyntaxName = synName

/// Picking names that do not conflict based on name hints.
[<AutoOpen>]
module private Diambiguation =

    type NetNameHint =
        {
            Namespace : NetName
            Name : NetId
            Suffix : option<NetId>
        }

        static member Create(name: NetName, suffix: NetId) : NetNameHint =
            match name.Parent with
            | None ->
                {
                    Namespace = name
                    Name = suffix
                    Suffix = None
                }
            | Some ns ->
                {
                    Namespace = ns
                    Name = name.Local
                    Suffix = Some suffix
                }

    let private MakeName (hint: NetNameHint) (attempt: int) : NetName =
        match attempt with
        | 0 -> hint.Namespace.[hint.Name]
        | k ->
            let suffix =
                match hint.Suffix with
                | Some x ->
                    match k with
                    | 1 -> string x
                    | _ -> string x + string (k - 1)
                | None ->
                    string k
            hint.Namespace.[hint.Name.Name + suffix]

    let private PickName (used: HashSet<NetName>) (hint: NetNameHint) : NetName =
        let rec pick n =
            let name = MakeName hint n
            if used.Add(name) then
                name
            else
                pick (n + 1)
        pick 0

    let Disambiguate<'T> (used: seq<NetName>) (hint: 'T -> NetNameHint) (input: seq<'T>) : Mp.Mapping<NetName,'T> =
        let input = Seq.toArray input
        let hints = Array.map hint input
        let namespaceNames =
            hints
            |> Seq.map (fun x -> x.Namespace)
        let usedNames = HashSet(Seq.append used namespaceNames)
        let pairs =
            input
            |> Seq.mapi (fun i x -> (PickName usedNames hints.[i], x))
        Mp.Mapping(pairs)

    let private MakeId (hint: NetId) (attempt: int) : NetId =
        match attempt with
        | 0 -> hint
        | k ->
            let suffix = string attempt
            NetId.Create(string hint + string suffix)

    let private PickId (used: HashSet<NetId>) (hint: NetId) : NetId =
        let rec pick n =
            let name = MakeId hint n
            if used.Add(name) then
                name
            else
                pick (n + 1)
        pick 0

    let DisambiguateLocal<'T> (inUse: seq<NetId>) (hint: 'T -> NetId) (input: seq<'T>) : Mp.Mapping<NetId,'T> =
        let used = HashSet(inUse)
        let pairs =
            input
            |> Seq.map (fun x -> (PickId used (hint x), x))
        Mp.Mapping(pairs)

type AssemblyDefinition =
    {
        Contracts : seq<Contract>
        Singletons : seq<Singleton>
        Start : NetName
    }

type private NamedDefinition =
    | NamedContract of Contract * NetName * Mp.Mapping<Key,NetId>
    | NamedSingleton of Singleton * NetName

/// Disambiguates names and constructs name mappings.
[<Sealed>]
type private NamedAssembly private (entryPoint: NetName, defs: seq<NamedDefinition>) =

    let contractNames =
        defs
        |> Seq.choose (function
            | NamedContract (c, n, _) -> Some (c, n)
            | _ -> None)
        |> dict

    let singletonNames =
        defs
        |> Seq.choose (function
            | NamedSingleton (c, n) -> Some (c, n)
            | _ -> None)
        |> dict

    member this.Name(c: Contract) = contractNames.[c]
    member this.Name(s: Singleton) = singletonNames.[s]
    member this.Definitions = defs
    member this.EntryPoint = entryPoint

    static member Create(def: AssemblyDefinition) : NamedAssembly =
        let hintContract (c: Contract) : NetNameHint =
            NetNameHint.Create(c.HintName, c.Suffix)
        let hintSingleton (s: Singleton) : NetNameHint =
            NetNameHint.Create(s.HintName, s.Suffix)
        let entryPoint = def.Start
        let defs =
            Seq.append
                (Seq.map Choice1Of2 def.Contracts)
                (Seq.map Choice2Of2 def.Singletons)
            |> Disambiguate [entryPoint] (function
                | Choice1Of2 x -> hintContract x
                | Choice2Of2 x -> hintSingleton x)
            |> Mp.GetPairs
            |> Seq.map (fun (name, entity) ->
                match entity with
                | Choice1Of2 contract ->
                    let used = name.List
                    let contractMap =
                        contract.Properties.GetKeys()
                        |> DisambiguateLocal used (fun x -> x.NetId())
                        |> Mp.GetPairs
                        |> Seq.map (fun (a, b) -> (b, a))
                        |> Mp.New
                    NamedContract (contract, name, contractMap)
                | Choice2Of2 singleton ->
                    NamedSingleton (singleton, name))
            |> Seq.cache
        NamedAssembly(entryPoint, defs)

/// Realizes the assembly definitions using System.Reflection.Emit.
module private Compilation =

    [<AutoOpen>]
    module private Util =

        type Kind =
            | Instance of Key
            | Static of S.Name

            member this.MethodAttributes =
                match this with
                | Instance _ -> MethodAttributes.Public
                | Static _ -> MethodAttributes.Public ||| MethodAttributes.Static

        let NotImplementedConstructor =
            typeof<NotImplementedException>.GetConstructor(Array.empty)

        let MakeNotImplemented (m: MethodBuilder) =
            let gen = m.GetILGenerator()
            gen.Emit(OpCodes.Newobj, NotImplementedConstructor)
            gen.Emit(OpCodes.Throw)
            gen.Emit(OpCodes.Ret)

        let RestAttributeBuilder : CustomAttributeBuilder =
            CustomAttributeBuilder(typeof<ParamArrayAttribute>.GetConstructor(Array.empty), Array.empty)

        let MakeRest (p: ParameterBuilder) =
            p.SetCustomAttribute(RestAttributeBuilder)

        let RegularClass =
            TypeAttributes.Class
            ||| TypeAttributes.Public
            ||| TypeAttributes.Sealed

        let NestedClass =
            TypeAttributes.Class
            ||| TypeAttributes.NestedPublic
            ||| TypeAttributes.Sealed

        let MakeMacroAttribute : System.Type -> CustomAttributeBuilder =
            let macroCtor = typeof<A.MacroAttribute>.GetConstructor([| typeof<System.Type> |])
            fun t -> CustomAttributeBuilder(macroCtor, [| box t |])

        let CallBuilder = MakeMacroAttribute typeof<WebSharper.Macros.CallMacro>
        let NewBuilder = MakeMacroAttribute typeof<WebSharper.Macros.NewMacro>
        let ItemBuilder = MakeMacroAttribute typeof<WebSharper.Macros.ItemMacro>

        let DefaultConstructorAttribute =
            let t = typeof<A.InlineAttribute>.GetConstructor([| typeof<string> |])
            CustomAttributeBuilder(t, [| box "{}" |])

        let MakeSingletonAttribute : Choice<S.Name,S.Identifier> -> list<CustomAttributeBuilder> =
            let nameCtor1 = typeof<A.NameAttribute>.GetConstructor([| typeof<string> |])
            let nameCtorN = typeof<A.NameAttribute>.GetConstructor([| typeof<string[]> |])
            let stubCtor = typeof<A.StubAttribute>.GetConstructor(Array.empty)
            let stubBuilder = CustomAttributeBuilder(stubCtor, Array.empty)
            fun name ->
                let arg =
                    match name with
                    | Choice1Of2 name ->
                        name.List
                        |> List.toArray
                        |> Array.map (fun x -> x.Name)
                        |> box
                    | Choice2Of2 id ->
                        box id.Name
                let nameCtor =
                    match name with
                    | Choice1Of2 _ -> nameCtorN
                    | Choice2Of2 _ -> nameCtor1
                [
                    CustomAttributeBuilder(nameCtor, [| arg |])
                    stubBuilder
                ]

    [<Sealed>]
    type private Builder(na: NamedAssembly, typesTable: M.Table<NetName,TypeBuilder>) =

        member this.BuildAttribute(kind: Kind) : list<CustomAttributeBuilder> =
            match kind with
            | Kind.Instance key ->
                match key with
                | Call -> [CallBuilder]
                | ItemByNumber -> [ItemBuilder]
                | ItemByString -> [ItemBuilder]
                | New -> [NewBuilder]
                | Property id -> MakeSingletonAttribute (Choice2Of2 id)
            | Kind.Static name -> MakeSingletonAttribute (Choice1Of2 name)

        member this.BuildContract(c: Contract, n: NetName, m: Mp.Mapping<Key,NetId>) =
            let self = typesTable.[n]
            for (key, prop) in c.Properties.GetPairs() do
                this.BuildMember(self, Instance key, m.[key], prop)

        member this.BuildMember(tb: TypeBuilder, kind: Kind, id: NetId, m: Member) : unit =
            match m with
            | OverloadedMember ov -> this.BuildMethod(tb, id, ov, kind)
            | TypedMember ty -> this.BuildProperty(tb, id, ty, Seq.empty, kind)

        member this.BuildMethod(tb: TypeBuilder, id: NetId, overloads: Overloads, kind: Kind) : unit =
            let attrs = kind.MethodAttributes
            for (ms, ret) in overloads.Variants do
                let ret =
                    let t = this.BuildType(ret)
                    if t = typeof<unit> then typeof<Void> else t
                let (ps, rest) = Signatures.Split ms
                let par =
                    [|
                        for p in ps do
                            yield this.BuildType(p)
                        match rest with
                        | Some t -> yield this.BuildType(t).MakeArrayType()
                        | None -> ()
                    |]
                let m = tb.DefineMethod(id.Name, attrs, ret, par)
                match rest with
                | Some _ -> MakeRest (m.DefineParameter(par.Length - 1, ParameterAttributes.None, "rest"))
                | None -> ()
                this.BuildAttribute(kind)
                |> List.iter m.SetCustomAttribute
                MakeNotImplemented m

        member this.BuildNamedDefinition(def: NamedDefinition) =
            match def with
            | NamedContract (x, y, z) -> this.BuildContract(x, y, z)
            | NamedSingleton (s, n) ->
                let parent = typesTable.[n.Parent.Value]
                this.BuildMember(parent, Static s.SyntaxName, n.Local, s.Member)

        member this.BuildProperty(tb: TypeBuilder, id: NetId, ty: Type, par: seq<Type>, kind: Kind) : unit =
            let attrs = kind.MethodAttributes
            let ty = this.BuildType(ty)
            let par = Array.map this.BuildType (Seq.toArray par)

            let smallName = id.Name

            let prop = tb.DefineProperty(id.Name, PropertyAttributes.None, ty, par)
            let gm = tb.DefineMethod("get_" + id.Name, attrs, ty, par)
            let sm = tb.DefineMethod("set_" + id.Name, attrs, typeof<Void>, Array.append par [| ty |])
            prop.SetGetMethod(gm)
            prop.SetSetMethod(sm)
            match kind with
            | Instance ItemByNumber
            | Instance ItemByString ->
                let attrs = this.BuildAttribute(kind)
                for a in attrs do
                    gm.SetCustomAttribute(a)
                    sm.SetCustomAttribute(a)
            |  _ ->
                this.BuildAttribute(kind)
                |> List.iter prop.SetCustomAttribute
            MakeNotImplemented gm
            MakeNotImplemented sm

        member this.BuildType(ty: Type) : System.Type =
            let (!) x = this.BuildType(x)
            match ty with
            | ArrayType x -> (!x).MakeArrayType()
            | AnyType -> typeof<obj>
            | BooleanType -> typeof<bool>
            | NumberType -> typeof<WebSharper.Number>
            | StringType -> typeof<string>
            | ContractType c -> typesTable.[na.Name(c.Value)] :> _
            | FunctionType ([], t) -> typedefof<_->_>.MakeGenericType(typeof<unit>, !t)
            | FunctionType ([x], t) -> typedefof<_->_>.MakeGenericType(!x, !t)
            | FunctionType (ts, t) ->
                let td = System.Type.GetType(String.Format("System.Tuple`{0}", ts.Length), true)
                let tt = td.MakeGenericType(Array.map (!) (List.toArray ts))
                typedefof<_->_>.MakeGenericType(tt, !t)
            | UnitType -> typeof<unit>

    let Compile (na: NamedAssembly) (mb: ModuleBuilder) : unit =
        let isOpenType =
            let defs =
                na.Definitions
                |> Seq.choose (function
                    | NamedContract (contract, name, mapping) ->
                        if contract.AllowsConstructor then Some name else None
                    | _ -> None)
            HashSet(defs).Contains
        let typeTable =
            let buildType (buildType: NetName -> TypeBuilder) (name: NetName) : TypeBuilder =
                let builder =
                    if name = na.EntryPoint then
                        mb.DefineType(name.Text, RegularClass)
                    else
                        let parent = buildType name.Parent.Value
                        parent.DefineNestedType(name.Local.Name, NestedClass)
                if not (isOpenType name) then
                    builder.DefineDefaultConstructor(MethodAttributes.Private)
                    |> ignore
                else
                    let ctor = builder.DefineDefaultConstructor(MethodAttributes.Public)
                    ctor.SetCustomAttribute(DefaultConstructorAttribute)
                builder
            M.MemoizeRecursive (M.Options()) buildType
        let builder = Builder(na, typeTable)
        for def in na.Definitions do
            builder.BuildNamedDefinition(def)
        for key in typeTable.GetKeys() do
            typeTable.[key].CreateType()
            |> ignore

    let private ConstructLock = obj ()

    let Construct (log: Logging.Log) (name: AssemblyName) (buildAssembly: ModuleBuilder -> unit) : byte [] =
        lock ConstructLock <| fun () ->
            let n = name.Name
            let folder = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
            Directory.CreateDirectory(folder) |> ignore
            try
                let aB = AppDomain.CurrentDomain.DefineDynamicAssembly(name, AssemblyBuilderAccess.Save, folder)
                let mB = aB.DefineDynamicModule(n, n, false)
                log.Time "buildAssembly" <| fun () ->
                    buildAssembly mB
                log.Time "AssemblyBuilder.Save()" <| fun () ->
                    aB.Save(n)
                File.ReadAllBytes(Path.Combine(folder, n))
            finally
                Directory.Delete(folder, true)

[<Sealed>]
type GeneratedAssembly(bytes: byte[], name: NetName) =
    member this.Write(s: Stream) = s.Write(bytes, 0, bytes.Length)
    member this.EntryPoint = name.Text

let Assemble (log: Logging.Log) (def: AssemblyDefinition) : GeneratedAssembly =
    let bytes =
        NamedAssembly.Create(def)
        |> Compilation.Compile
        |> Compilation.Construct log (AssemblyName(def.Start.Text))
    GeneratedAssembly(bytes, def.Start)
