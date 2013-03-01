/// Adapts the results of the `Elaboration` pass to the structure expected
/// by `Assembling`.  Basically, discoveres/merges contracts and singletons,
/// detects function-like types, computes name hints, constructs named types for
/// anonymous types inline types found by `Elaboration`.
module internal IntelliFactory.TypeScript.Adaptation

open System.Collections.Generic
module A = Assembling
module E = Elaboration
module L = Logging
module M = Memoization
module Mp = Mappings
module N = NameResolution
module S = Syntax

/// Uniquely identifies explicitly named/declared contracts.
type ContractIdentity =

    /// Module contract for the implied global module.
    | Global

    /// Contract for module-or-type at location.
    | Located of E.Location

    /// Static contract for the class or enum at location.
    | Statics of E.Location

    static member FromContext(ctx: E.Context) =
        match ctx with
        | E.Global -> Global
        | E.At loc -> Located loc

/// Adapted `E.Fact` information, after name resolution.
type Fact =
    | ContractExtends of ContractIdentity * ContractIdentity
    | ContractSubsumes of ContractIdentity * E.Struct
    | IsA of ContractIdentity * N.EntityKind

    member this.Identity =
        match this with
        | ContractSubsumes (id, _)
        | ContractExtends (id, _)
        | IsA (id, _) -> id

/// Adapts `E.Fact` sequence ot `Fact` sequence, primarily doing name resolution.
let AdaptFacts (trace: L.Log) (nr: N.INameResolver) (facts: seq<E.Fact>) : seq<Fact> =
    let unresolved (ctx: obj) (name: obj) =
        trace.Warning("Unresolved name: {0} in {1}", name, ctx)
    seq {
        yield IsA (Global, N.IsModule)
        for f in facts do
            match f with
            | E.ClassInherits (loc, ctx, name) ->
                match nr.ResolveName(ctx, name) with
                | None -> unresolved ctx name
                | Some x ->
                    yield ContractExtends (Located loc, Located x.CanonicalLocation)
            | E.ClassStaticsInclude (loc, str) ->
                yield ContractSubsumes (Statics loc, str)
            | E.ContractIncludes (ctx, str) ->
                yield ContractSubsumes (ContractIdentity.FromContext ctx, str)
            | E.ImportExternal (id, ctx, path) ->
                let t = E.LocatedType (E.Extern path)
                let c = E.Struct.Singleton(id, E.Required t)
                yield ContractSubsumes (ContractIdentity.FromContext ctx, c)
            | E.ImportInternal (id, ctx, name) ->
                match nr.ResolveName(ctx, name) with
                | None -> unresolved ctx name
                | Some x ->
                    let str = E.Struct.Singleton (id, E.Required (E.Type.LocatedType x.CanonicalLocation))
                    yield ContractSubsumes (ContractIdentity.FromContext ctx, str)
            | E.InterfaceExtends (loc, ctx, name) ->
                match nr.ResolveName(ctx, name) with
                | None -> unresolved ctx name
                | Some x ->
                    yield ContractExtends (Located loc, Located x.CanonicalLocation)
            | E.IsModule loc ->
                yield IsA (Located loc, N.IsModule)
            | E.PathIsDefined path ->
                yield IsA (Located (E.Extern path), N.IsModule)
            | E.IsClass loc ->
                yield IsA (Located loc, N.IsClass)
            | E.IsEnum loc ->
                yield IsA (Located loc, N.IsEnum)
            | E.IsInterface loc ->
                yield IsA (Located loc, N.IsInterface)
    }

type RawContract =
    {
        Identity : ContractIdentity
        Kind : N.EntityKind
        Struct : E.Struct
    }

/// Resolves inherit/extend relationships by member copying.
let ProcessFacts (facts: seq<Fact>) : Mp.Mapping<ContractIdentity,RawContract> =
    let raw =
        facts
        |> Seq.choose (function
            | ContractSubsumes (id, str) -> Some (id, str)
            | _ -> None)
        |> Seq.groupBy fst
        |> Seq.map (fun (id, rest) ->
            (id, E.Struct.Union (Seq.map snd rest)))
        |> dict
    let kinds =
        facts
        |> Seq.choose (function
            | IsA (id, kind) -> Some (id, kind)
            | _ -> None)
        |> dict
    let extensions =
        facts
        |> Seq.choose (function
            | ContractExtends (loc, other) -> Some (loc, other)
            | _ -> None)
        |> Seq.groupBy fst
        |> Seq.map (fun (loc, rest) ->
            let rest = Seq.map snd rest
            (loc, rest))
        |> dict
    let identities = HashSet(facts |> Seq.map (fun f -> f.Identity))
    seq {
        for id in identities do
            let str : E.Struct =
                let visited = HashSet()
                let output = Queue()
                let rec visit (id: ContractIdentity) =
                    if visited.Add(id) then
                        match raw.TryGetValue(id) with
                        | true, s -> output.Enqueue(s)
                        | _ -> ()
                        match extensions.TryGetValue(id) with
                        | true, exts ->
                            for ext in exts do
                                visit ext
                        | _ -> ()
                visit id
                E.Struct.Union output
            let c : RawContract =
                {
                    Identity = id
                    Kind =
                        match kinds.TryGetValue(id) with
                        | true, k -> k
                        | _ -> N.IsModule
                    Struct = str
                }
            yield (id, c)
    }
    |> Mp.New

/// Detects structs that only have call members.
let (|CallType|_|) (s: E.Struct) : option<E.Overloads> =
    let ok =
        not s.Call.Variants.IsEmpty
        && s.Construct.IsEmpty
        && s.IndexByNumber.IsNone
        && s.IndexByString.IsNone
        && s.Properties.Count = 0
    if ok then Some s.Call else None

/// Detects function-like structs.
let (|FuncType|_|) (s: E.Struct) : option<list<E.Type> * E.Type> =
    let ok =
        s.Call.Variants.Length = 1
        && s.Construct.IsEmpty
        && s.IndexByNumber.IsNone
        && s.IndexByString.IsNone
        && s.Properties.Count = 0
    if ok then
        let ov = s.Call.Variants.[0]
        let (ps, rest) = Signatures.Split ov.Signature
        match rest with
        | None -> Some (ps, ov.Return)
        | _ -> None
    else None

/// Tests if all properties are optional.
let IsOpen (str: E.Struct) =
    str.Call.IsEmpty
    && str.Construct.IsEmpty
    && str.IndexByNumber.IsNone
    && str.IndexByString.IsNone
    &&  Mp.GetPairs str.Properties
        |> Seq.forall (fun (_, v)->
            match v with
            | E.Optional _ -> true
            | _ -> false)

/// Shared pre-constructed identifier literals.
module Id =
    let Class = A.NetId.Create "Class"
    let Contract = A.NetId.Create "Contract"
    let Enum = A.NetId.Create "Enum"
    let Extern = A.NetId.Create "Extern"
    let Global = A.NetId.Create "Global"
    let Helper = A.NetId.Create "Helper"
    let Instance = A.NetId.Create "Instance"
    let Interface = A.NetId.Create "Interface"
    let Module = A.NetId.Create "Module"
    let Singleton = A.NetId.Create "Singleton"
    let Static = A.NetId.Create "Static"

/// Does the dirty work of converting `RawStruct` to `A.Contract`.
module Building =

    type Config =
        {
            Add : A.Contract -> A.Type
            Find : E.Location -> A.Type
            NameResolver : N.INameResolver
            Trace : L.Log
        }

    [<Sealed>]
    type private Builder(config: Config, hint: A.NetName) =

        member this.BuildType(t: E.Type) : A.Type =
            match t with
            | E.AnyType -> A.AnyType
            | E.ArrayType t -> A.ArrayType (this.BuildType t)
            | E.BooleanType -> A.BooleanType
            | E.NumberType -> A.NumberType
            | E.StructType str -> this.BuildAnonymousInterface(str)
            | E.StringType -> A.StringType
            | E.LocatedType loc -> config.Find loc
            | E.NamedType (ctx, name) ->
                match config.NameResolver.ResolveName(ctx, name) with
                | None ->
                    config.Trace.Warning("Failed to resolve name {0} in context {1}", name, ctx)
                    A.AnyType
                | Some entity ->
                    config.Find entity.CanonicalLocation
            | E.UnitType -> A.UnitType

        member this.BuildStruct(str: E.Struct) : Mp.Mapping<A.Key,A.Member> =
            seq {
                if not str.Call.IsEmpty then
                    yield (A.Call, A.OverloadedMember (this.BuildOverloads str.Call))
                if not str.Construct.IsEmpty then
                    yield (A.New, A.OverloadedMember (this.BuildOverloads str.Construct))
                yield! this.BuildIndexers(str)
                for (k, v) in Mp.GetPairs str.Properties do
                    yield (A.Property k, this.BuildProperty v.Type)
            }
            |> Mp.New

        member this.BuildProperty(t: E.Type) : A.Member =
            match t with
            | E.StructType (CallType ov) -> A.OverloadedMember (this.BuildOverloads ov)
            | t -> A.TypedMember (this.BuildType(t))

        member this.BuildSignature(s: E.Signature) : A.Signature =
            Signatures.Map this.BuildType s

        member this.BuildOverload(ov: E.Overload) : A.Signature * A.Type =
            (this.BuildSignature ov.Signature, this.BuildType ov.Return)

        member this.BuildOverloads(ss: E.Overloads) : A.Overloads =
            let overloads = [ for ov in ss.Variants -> this.BuildOverload ov ]
            A.Overloads(overloads)

        member this.BuildIndexers(str: E.Struct) =
            let build key t =
                match t with
                | None -> []
                | Some t -> [key, A.TypedMember (this.BuildType t)]
            build A.ItemByNumber str.IndexByNumber @ build A.ItemByString str.IndexByString

        member this.BuildAnonymousInterface(str: E.Struct) : A.Type =
            match str with
            | FuncType (ps, ret) ->
                A.FunctionType (List.map this.BuildType ps, this.BuildType ret)
            | str ->
                A.Contract {
                    AllowsConstructor = IsOpen str
                    HintName = hint.[Id.Helper]
                    Properties = this.BuildStruct(str)
                    Suffix = Id.Interface
                }
                |> config.Add

    let Build (config: Config) (hint: A.NetName) (suffix: A.NetId) (rc: RawContract) : A.Contract =
        A.Contract {
            AllowsConstructor =
                match rc.Kind with
                | N.IsClass | N.IsEnum -> false
                | _ -> IsOpen rc.Struct
            HintName = hint
            Properties = Builder(config, hint).BuildStruct(rc.Struct)
            Suffix = suffix
        }

type ExternalName =
    {
        Net : A.NetName
        Syntax : S.Name
    }

//let AddSuffix (n: A.NetId) (name: A.NetName) =
//    let r = A.NetId.Create (name.Local.Name + n.Name)
//    match name.Parent with
//    | None -> A.NetName.Global r
//    | Some p -> p.[r]

[<Sealed>]
type NameHintAdapter private (start: A.NetName, ptn: IDictionary<S.Path,ExternalName>) =

    let convertScope (s: E.Scope) : A.NetName =
        match s with
        | E.GlobalScope -> start
        | E.ExternScope path ->
            match ptn.TryGetValue(path) with
            | true, n -> n.Net
            | _, _ -> start

    let getSuffix (kind: N.EntityKind) =
        match kind with
        | N.IsClass -> Id.Class
        | N.IsEnum -> Id.Enum
        | N.IsInterface -> Id.Interface
        | N.IsModule -> Id.Module

    member this.GetHint(rc: RawContract, forContract: bool) : A.NetName * A.NetId =
        let atLoc (loc: E.Location) =
            match loc with
            | E.Local (scope, name) -> (convertScope scope).[Symbols.ConvertName(name)]
            | E.Extern p -> convertScope (E.ExternScope p)
        match rc.Identity with
        | Global -> (start, Id.Global)
        | Located loc ->
            match rc.Kind with
            | N.IsModule when forContract -> ((atLoc loc).[Id.Module], Id.Contract)
            | N.IsClass | N.IsEnum when forContract -> ((atLoc loc).[Id.Contract], Id.Class)
            | _ -> (atLoc loc, getSuffix rc.Kind)
        | Statics loc ->
            if forContract
                then ((atLoc loc).[Id.Static], getSuffix rc.Kind)
                else (atLoc loc, getSuffix rc.Kind)

    member this.GetModuleOrigin(c: ContractIdentity) : option<S.Name> =
        let scope path =
            match ptn.TryGetValue(path) with
            | true, x -> Some x.Syntax
            | _ -> None
        match c with
        | Global -> None
        | Located (E.Extern p) | Statics (E.Extern p) -> scope p
        | Located (E.Local (sc, name)) | Statics (E.Local (sc, name)) ->
            match sc with
            | E.GlobalScope -> Some name
            | E.ExternScope path ->
                match scope path with
                | Some r -> Some r.[name]
                | None -> None

    static member Create(start: A.NetName)(facts: seq<E.Fact>) =
        let pathToName =
            seq {
                for f in facts do
                    match f with
                    | E.Fact.ImportExternal (id, ctx, path) ->
                        match ctx.[id] with
                        | E.Local (E.GlobalScope, name) ->
                            let sn = Symbols.ConvertName name
                            yield (path, { Net = start.[sn]; Syntax = name })
                        | _ -> ()
                    | _ -> ()
            }
            |> dict
        NameHintAdapter(start, pathToName)

let Adapt (trace: L.Log) (start: A.NetName) (file: S.DeclarationSourceFile) : A.AssemblyDefinition =
    let nR = N.ConstructResolver trace file
    let facts = E.Elaborate trace file
    let contractMap =
        AdaptFacts trace nR facts
        |> ProcessFacts
    let contracts = Queue()
    let singletons = Queue()
    let hints = NameHintAdapter.Create start facts
    let addContract (c: A.Contract) : A.Type =
        contracts.Enqueue(c)
        A.ContractType (Lazy.CreateFromValue c)
    let buildRawContract (config: Building.Config) (rc: RawContract) : A.Contract =
        let (hint, suffix) = hints.GetHint(rc, true)
        let c = Building.Build config hint suffix rc
        match rc.Identity with
        | Global -> ()
        | _ -> contracts.Enqueue(c)
        match rc.Kind with
        | N.IsModule ->
            let (moduleOrigin, getPropertyOrigin) =
                match hints.GetModuleOrigin rc.Identity with
                | Some moduleOrigin ->
                    (Some moduleOrigin, Some (function
                        | A.Property id -> Some moduleOrigin.[id]
                        | _ -> Some moduleOrigin))
                | None ->
                    match rc.Identity with
                    | Global ->
                        let f = function
                            | A.Property id -> Some (S.Name.Global id)
                            | _ -> None
                        (None, Some f)
                    | _ ->
                        (None, None)
            match moduleOrigin with
            | None -> ()
            | Some moduleOrigin ->
                let (hint, suffix) = hints.GetHint(rc, false)
                let ty = A.ContractType (Lazy.CreateFromValue c)
                A.Singleton (hint.[Id.Instance], moduleOrigin, A.TypedMember ty, Id.Module)
                |> singletons.Enqueue
            match getPropertyOrigin with
            | None -> ()
            | Some gpo ->
                let (hint, suffix) = hints.GetHint(rc, false)
                for (key, prop) in Mp.GetPairs c.Properties do
                    match gpo key with
                    | Some origin ->
                        let propHint = hint.[key.NetId()]
                        A.Singleton (propHint, origin, prop, Id.Singleton)
                        |> singletons.Enqueue
                    | None -> ()
        | _ -> ()
        c
    let typeTable : M.Table<ContractIdentity,Lazy<A.Contract>> =
        let opts : M.Options<ContractIdentity,Lazy<A.Contract>> =
            M.Options(Laziness = Some (fun x -> lazy x.Value.Value))
        M.MemoizeRecursive opts (fun recur ->
            let getType : E.Location -> A.Type = fun loc ->
                A.ContractType (recur (Located loc))
            let buildConfig : Building.Config =
                {
                    Add = addContract
                    NameResolver = nR
                    Find = getType
                    Trace = trace
                }
            fun id -> lazy buildRawContract buildConfig contractMap.[id])
    for (k, _) in Mp.GetPairs contractMap do
        typeTable.[k].Value |> ignore
    {
        Contracts = contracts.ToArray() :> seq<_>
        Singletons = singletons.ToArray() :> seq<_>
        Start = start
    }
