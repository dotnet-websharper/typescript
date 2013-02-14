/// Translates syntax to assembly.
module internal IntelliFactory.TypeScript.Translation

open System.Collections.Generic
module A = Assembling
module C = Contexts
module D = TypeDiscovery
module M = Memoization
module N = NameResolution
module S = Syntax
module SR = SyntaxReductions
type private NN = A.NetNames.NetName

let IsOptional (m: S.TypeMember) =
    match m with
    | S.CallMember _ -> false
    | S.ConstructMember _ -> false
    | S.FunctionMember (S.FunctionCallSignature _) -> false
    | S.FunctionMember (S.FunctionSignature (_, S.Optional, _, _)) -> true
    | S.FunctionMember (S.FunctionSignature _) -> false
    | S.IndexMember _ -> false
    | S.PropertyMember (S.PropertySignature (_, S.Optional, _)) -> true
    | S.PropertyMember (S.PropertySignature (_, _, _)) -> false

let rec ContextToName (ctx: C.Context) (ending: option<S.Identifier>) : S.Name =
    match ctx with
    | C.External _ | C.Global -> S.Name.Global (defaultArg ending (S.Identifier.Create "Global"))
    | C.Nested (parent, id) ->
        match ending with
        | None -> ContextToName parent (Some id)
        | Some e -> (ContextToName parent (Some id)).Local(e)

type State() =
    let contracts = Queue<A.Contract>()
    let singletons = Queue<A.Singleton>()

    member this.AddContract(c: A.Contract) =
        contracts.Enqueue(c)

    member this.AddSingleton(s: A.Singleton) =
        singletons.Enqueue(s)

    member this.Build(start) : A.AssemblyDefinition =
        {
            Contracts = contracts.ToArray() :> seq<_>
            Singletons = singletons.ToArray() :> seq<_>
            Start = start
        }

[<Sealed>]
type Builder(state: State, log: Logging.Log, ctx: C.Context, nr: N.INameResolver, getType: D.DiscoveredType -> A.Type) =

    member this.BuildType(t: S.Type) : A.Type =
        match t with
        | S.AnyType -> A.BuiltInType A.AnyType
        | S.ArrayOf t -> A.ArrayType (this.BuildType t)
        | S.BoolType -> A.BuiltInType A.BooleanType
        | S.NumberType -> A.BuiltInType A.NumberType
        | S.ObjectType ms -> this.BuildAnonymousInterface(ms)
        | S.StringType -> A.BuiltInType A.StringType
        | S.TypeName name ->
            match nr.ResolveName(ctx, name) with
            | None ->
                log.Warning("Failed to resolve name {0} in context {1}", name, ctx)
                A.BuiltInType A.AnyType
            | Some entity ->
                getType entity

    member this.BuildMember(t: S.Type) : A.Member =
        match t with
        | SR.CallType variants ->
            let ov = this.BuildOverloads(variants)
            A.OverloadedMember ov
        | _ ->
            A.TypedMember (this.BuildType(t))

    member this.BuildProperty(SR.Node (key, ty): SR.Node) : A.Key * A.Member =
        let m = this.BuildMember(ty)
        (key, m)

    member this.BuildProperties(ms: seq<S.TypeMember>) =
        ms
        |> Seq.choose (function
            | S.FunctionMember (S.FunctionSignature _ as fs) -> Some (SR.FunctionNode fs)
            | S.PropertyMember prop -> Some (SR.PropertyNode prop)
            | _ -> None)
        |> Seq.map this.BuildProperty
        |> Seq.cache

    member this.BuildTypeMembers(ms: seq<S.TypeMember>) : Mapping<A.Key,A.Member> =
        let props =
            let call = this.BuildCall(ms)
            let construct = this.BuildConstruct(ms)
            seq {
                if not call.IsEmpty then
                    yield (A.Call, A.OverloadedMember call)
                if not construct.IsEmpty then
                    yield (A.New, A.OverloadedMember call)
                yield! this.BuildProperties(ms)
                yield! this.BuildIndexers(ms)
            }
        Mapping(props)

    member this.BuildParameter(p: S.Parameter) =
        match p with
        | S.Parameter (_, ty) -> this.BuildType(ty)

    member this.BuildSignature(ps: S.Parameters) : list<A.Signature> =
        let rest =
            match ps.RestParameter with
            | None -> Signatures.Empty
            | Some (S.Parameter (_, t)) -> Signatures.Rest (this.BuildType(t))
        let opt = ps.OptionalParameters |> Seq.map this.BuildParameter |> Seq.toList
        let req = ps.RequiredParameters |> Seq.map this.BuildParameter |> Seq.toList
        let cons x xs = Signatures.With(x, xs)
        [
            for opt in List.Inits opt ->
                rest
                |> List.foldBack cons opt
                |> List.foldBack cons req
        ]

    member this.BuildOverloads(ss: seq<S.Parameters * S.Return>) =
        let overloads =
            ss
            |> Seq.collect (fun (ps, ret) ->
                let ret =
                    match ret with
                    | S.Returns x -> this.BuildType x
                    | S.Void -> A.UnitType
                let ss = this.BuildSignature(ps)
                [ for s in ss -> (s, ret) ])
            |> Seq.toList
        A.Overloads(overloads)

    member this.BuildCall(ms: seq<S.TypeMember>) : A.Overloads =
        ms
        |> Seq.choose SR.(|CallMember|_|)
        |> this.BuildOverloads

    member this.BuildConstruct(ms: seq<S.TypeMember>) : A.Overloads =
        ms
        |> Seq.choose (function
            | S.ConstructMember (S.ConstructSignature (ps, ty)) -> Some (ps, S.Returns ty)
            | _ -> None)
        |> this.BuildOverloads

    member this.BuildIndexers(ms: seq<S.TypeMember>) =
        ms
        |> Seq.choose (function
            | S.IndexMember (S.IndexSignature (S.Parameter (name, t1), t2)) ->
                match t1 with
                | S.StringType ->
                    Some (A.ItemByString, A.TypedMember (this.BuildType(t2)))
                | S.NumberType ->
                    Some (A.ItemByNumber, A.TypedMember (this.BuildType(t2)))
                | _ ->
                    log.Warning("Invalid indexer: only string and number supported")
                    None
            | _ -> None)
        |> Seq.distinctBy fst
        |> Seq.cache

    member this.BuildAnonymousInterface(ms: seq<S.TypeMember>) : A.Type =
        let props = this.BuildTypeMembers(ms)

        let isFunc =
            props.GetPairs()
            |> Seq.forall (function
                | (A.Call, A.OverloadedMember ov) ->
                    let vs = ov.Variants
                    vs.Length = 1 &&
                        let (_, rest) = Signatures.Split (fst vs.[0])
                        rest.IsNone
                | _ ->
                    false)
        if isFunc then
            let (s, ret) =
                match props.[A.Call] with
                | A.OverloadedMember ov -> ov.Variants.[0]
                | _ -> failwith "Impossible"
            let (par, _) = Signatures.Split s
            match par with
            | [] -> A.FunctionType (A.UnitType, ret)
            | [x] -> A.FunctionType (x, ret)
            | _ -> A.FunctionType (A.TupleType par, ret)
        else
            let isOpen = Seq.forall IsOptional ms
            let c = A.Contract(ContextToName ctx (Some (S.Identifier.Create "Helper")), props, None, isOpen)
            state.AddContract(c)
            A.ContractType (Delayed (Lazy.CreateFromValue c))

    member this.BuildInterface(i: D.Interface)  =
        let isOpen = Seq.forall IsOptional i.Members
        let props = this.BuildTypeMembers(i.Members)
        A.Contract(ContextToName ctx None, props, Some (A.NetId.Create "Interface"), isOpen)

    member this.BuildDiscoveredType(dty: D.DiscoveredType) : Delayed<A.Contract> =
        let make () =
            match dty with
            | D.DiscoveredClass c -> failwith "TODO"
            | D.DiscoveredInterface i -> this.BuildInterface(i)
            | D.DiscoveredEnum e -> failwith "TODO"
            | D.DiscoveredModule m -> A.Contract(ContextToName ctx None, Mapping(), Some (A.NetId.Create "Module"), false)
        Delayed(Lazy.Create(make))

    member this.BuildSingleton(ctx: C.Context, node: SR.Node) : unit =
        match node with
        | SR.Node (key, ty) ->
            let id = key.Id
            let n = ContextToName (ctx.RelativeContext(id)) None
            A.Singleton (n, this.BuildMember ty, None)
            |> state.AddSingleton

let Translate (log: Logging.Log) (nr: N.INameResolver) (discovered: seq<D.DiscoveredEntity>) (main: NN) : A.AssemblyDefinition =
    let state = State()
    let typeTable : M.Table<D.DiscoveredType,Delayed<A.Contract>> =
        let delay (a: Lazy<Delayed<A.Contract>>) : Delayed<A.Contract> =
            Delayed(lazy a.Value.Value)
        let cmp : System.Collections.Generic.IEqualityComparer<D.DiscoveredType> =
            HashIdentity.FromFunctions
                (fun x -> hash x.Location)
                (fun x y -> x.Location = y.Location)
        M.MemoizeRecursive (M.Options(Equality = cmp, Laziness = Some delay)) (fun recur ->
            let getType : D.DiscoveredType -> A.Type = fun t -> A.ContractType (recur t)
            fun dty ->
                let ctx = dty.Location.ToContext()
                Builder(state, log, ctx, nr, getType).BuildDiscoveredType(dty))
    for d in discovered do
        match d with
        | D.DiscoveredImportExternal _ -> ()
        | D.DiscoveredImportInternal _ -> ()
        | D.DiscoveredTypeEntity dty ->
            typeTable.[dty]
            |> ignore
        | D.DiscoveredGlobal (ctx, g) ->
            let b = Builder(state, log, ctx, nr, fun x -> A.ContractType (typeTable.[x]))
            b.BuildSingleton(ctx, g)
    for x in typeTable.GetKeys() do
        state.AddContract typeTable.[x].Value
    state.Build(main)

