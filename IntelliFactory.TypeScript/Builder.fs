/// Builds WebSharper interface assemblies based on Declarations files.
module IntelliFactory.TypeScript.Declarations.Builder

open System
open System.Collections.Generic
open System.IO
open IntelliFactory.TypeScript
module S = Syntax

module R = Reduced

let rec inits (el: list<'T>) : list<list<'T>> =
    match el with
    | [] -> [[]]
    | x :: xs -> [] :: [for rest in inits xs -> x :: rest]

let rec explode (interp: 'T -> list<'R>) (input: list<'T>) : list<list<'R>> =
    match input with
    | [] -> [[]]
    | x :: xs ->
        [
            for h in interp x do
                for t in explode interp xs do
                    yield h :: t
        ]

type MethodVariant =
    | CallInstance
    | CallStatic of S.Name
    | Construct
    | Constructor of S.Name
    | RegularInstance of S.Identifier
    | RegularInterface of S.Identifier
    | RegularStatic of S.Name

module Attribute =

    let Stub : CSharp.Attribute =
        CSharp.Name.Create("IntelliFactory", "WebSharper", "Core", "Attributes", "StubAttribute")
        |> CSharp.Type.Named
        |> CSharp.Attribute.Create

    let private inlineType : CSharp.Type =
        CSharp.Name.Create("IntelliFactory", "WebSharper", "Core", "Attributes", "InlineAttribute")
        |> CSharp.Type.Named

    let Inline (x: string) : CSharp.Attribute =
        CSharp.Attribute.Create(inlineType, x)

type Inline =
    | InlineCallInstance of option<S.Identifier> * int
    | InlineCallStatic of S.Name * int
    | InlineConstruct of int
    | InlineConstructor of S.Name * int
    | InlineField of S.Name
    | InlineIndexGet
    | InlineIndexSet

    member this.GetString() =
        match this with
        | InlineCallInstance (n, k) ->
            let i = Inline.Generate(CSharp.Instance, k)
            match n with
            | None -> String.Format("$0({0})", i)
            | Some n -> String.Format("$0.{0}({1})", n, i)
        | InlineCallStatic (name, k) ->
            String.Format("{0}({1})", name, Inline.Generate(CSharp.Static, k))
        | InlineConstruct k ->
            String.Format("new $0({0})", Inline.Generate(CSharp.Instance, k))
        | InlineConstructor (name, k) ->
            String.Format("new {0}({1})", name, Inline.Generate(CSharp.Static, k))
        | InlineField f -> string f
        | InlineIndexGet -> "$0[$1]"
        | InlineIndexSet -> "void ($0[$1]=$2)"

    member this.GetAttribute() : CSharp.Attribute =
        Attribute.Inline (this.GetString())

    override this.ToString() =
        this.GetString()

    static member Generate(kind: CSharp.MemberKind, arity: int) : string =
        let d =
            match kind with
            | CSharp.Static -> 0
            | CSharp.Instance -> 1
        seq {
            for i in 0 .. arity - 1 ->
                let k = i + d
                String.Format("${0}", k)
        }
        |> String.concat ", "

let ( |S1|_| ) x =
    match Seq.toArray x with
    | [| x |] -> Some x
    | _ -> None

type Context =
    {
        AnonymousTypes : ResizeArray<CSharp.Id * CSharp.Interface>
        Module : R.Module
        QualifiedName : option<S.Name>
    }

    member this.AddAnonymousType(key, ty) =
        this.AnonymousTypes.Add(key, ty)

    member this.BuildAnonymousType(defs: seq<R.InterfaceMember>, prefix: option<string>) : CSharp.Type =
        let name = CSharp.Id.Create(defaultArg prefix "" + "Contract")
        let result : CSharp.Interface =
            {
                Attributes = []
                Extends = []
                InterfaceMembers =
                    defs
                    |> Seq.map this.BuildInterfaceMember
                    |> CSharp.Scope.Build
                Type = CSharp.Type.Fresh()
            }
        this.AddAnonymousType(name, result)
        result.Type

    member this.BuildClassMember(n: S.Name, m: R.ClassMember) : CSharp.Id * CSharp.ClassMember =
        match m with
        | R.ClassConstructor ss ->
            let ss = [| for x in ss -> R.Signature.Create(x) |]
            this.BuildMethod(Constructor n, ss, CSharp.ClassConstructor)
        | R.ClassMethod (kind, id, ss) ->
            let v =
                match kind with
                | CSharp.Static -> RegularStatic n.[id]
                | CSharp.Instance -> RegularInstance id
            this.BuildMethod(v, ss, fun o -> CSharp.ClassMethod(kind, o))
        | R.ClassProperty (kind, id, ty) ->
            this.BuildProperty([Attribute.Stub], id, ty, fun x ->
                CSharp.ClassProperty(kind, x))

    member this.BuildFunType(defs: seq<R.InterfaceMember>) : option<CSharp.Type> =
        match defs with
        | S1 (R.InterfaceCall (S1 s)) when
            s.Parameters.RestParameter.IsNone
            && Seq.isEmpty s.Parameters.OptionalParameters ->
            let rangeType =
                match s.Return with
                | S.Returns ret -> this.BuildType ret
                | S.Void -> CSharp.Type.Unit
            let s = this.BuildSignature s.Parameters.RequiredParameters
            let domainType =
                match Seq.toArray s.Parameters with
                | [| |] -> CSharp.Type.Unit
                | [| x |] -> x.Type
                | xs ->
                    let ps = Array.sub xs 1 (xs.Length - 1)
                    let ts = [| for p in ps -> p.Type |]
                    CSharp.Type.Tuple(xs.[0].Type, ts)
            Some (CSharp.Type.Fun (domainType, rangeType))
        | _ -> None

    member this.BuildFunTypes(defs: seq<R.InterfaceMember>) : option<list<CSharp.Type>> =
        match defs with
        | S1 (R.InterfaceCall (S1 s)) when
            s.Parameters.RestParameter.IsNone
            && Seq.isEmpty s.Parameters.OptionalParameters ->
            let rangeType =
                match s.Return with
                | S.Returns ret -> this.BuildType ret
                | S.Void -> CSharp.Type.Unit
            Some [
                for s in this.BuildSignatures s.Parameters do
                    let domainType =
                        match Seq.toArray s.Parameters with
                        | [| |] -> CSharp.Type.Unit
                        | [| x |] -> x.Type
                        | xs ->
                            let ps = Array.sub xs 1 (xs.Length - 1)
                            let ts = [| for p in ps -> p.Type |]
                            CSharp.Type.Tuple(xs.[0].Type, ts)
                    yield CSharp.Type.Fun (domainType, rangeType)
            ]
        | _ -> None

    member this.BuildInterfaceMember(m: R.InterfaceMember) : CSharp.Id * CSharp.InterfaceMember =
        match m with
        | R.InterfaceCall ss ->
            this.BuildMethod(CallInstance, ss, CSharp.InterfaceMethod)
        | R.InterfaceConstruct ss ->
            this.BuildMethod(Construct, ss, CSharp.InterfaceMethod)
        | R.InterfaceIndexer ss ->
            let ov =
                CSharp.IndexerOverloads.Create <|
                    seq {
                        for (id, t1, t2) in ss do
                            let t2 = this.BuildType(t2)
                            for t1 in this.BuildTypes(t1) do
                                yield {
                                    Attributes = []
                                    Parameter = CSharp.Parameter.Create(id, t1)
                                    Type = t2
                                }
                    }
            let p = CSharp.InterfaceIndex ov
            (CSharp.Id.Create(S.Identifier "Item"), p)
        | R.InterfaceMethod (id, ss) ->
            this.BuildMethod(RegularInterface id, ss, CSharp.InterfaceMethod)
        | R.InterfaceProperty (id, ty) ->
            this.BuildProperty([], id, ty, CSharp.InterfaceProperty)

    member this.BuildMethod(variant: MethodVariant, ss: seq<R.Signature>) : CSharp.Id * CSharp.Overloads =
        let kind =
            match variant with
            | CallInstance | Construct | RegularInstance _ | RegularInterface _ -> CSharp.Instance
            | CallStatic _ | Constructor _ | RegularStatic _ -> CSharp.Static
        let ident =
            match variant with
            | CallInstance
            | CallStatic _ -> S.Identifier "Call"
            | Construct -> S.Identifier "New"
            | Constructor _ -> S.Identifier "Create"
            | RegularInstance m -> m
            | RegularInterface m -> m
            | RegularStatic m -> m.Name
            |> CSharp.Id.Create
        let ov =
            CSharp.Overloads.Build [
                for mS in ss do
                    let ret =
                        match mS.Return with
                        | S.Returns x -> Some (this.BuildType(x))
                        | _ -> None
                    for s in this.BuildSignatures(mS.Parameters) do
                        let i =
                            match variant with
                            | RegularInterface _ -> []
                            | CallInstance -> [InlineCallInstance (None, s.Arity)]
                            | CallStatic n -> [InlineCallStatic (n, s.Arity)]
                            | Construct -> [InlineConstruct s.Arity]
                            | Constructor name -> [InlineConstructor (name, s.Arity)]
                            | RegularInstance id -> [InlineCallInstance (Some id, s.Arity)]
                            | RegularStatic n -> [InlineCallStatic(n, s.Arity)]
                        yield (s, ret, [for x in i -> x.GetAttribute()])
            ]
        (ident, ov)

    member this.BuildMethod<'T>(v: MethodVariant, ss: seq<R.Signature>, k: CSharp.Overloads -> 'T) : CSharp.Id * 'T =
        let (id, x) = this.BuildMethod(v, ss)
        (id, k x)

    member this.BuildModule() : CSharp.Class =
        {
            Attributes = []
            ClassMembers =
                let mainScope =
                    CSharp.Scope.Build [|
                        for x in this.Module.Members do
                            yield this.BuildModuleMember x
                        for (k, v) in this.Module.Modules do
                            let ctx : Context = this.GetNestedContext(k, v)
                            let m = CSharp.ClassNested (ctx.BuildModule())
                            yield (CSharp.Id.Create(k), m)
                        for t in this.Module.TypeDefinitions do
                            let r = this.BuildTypeDefinition(t)
                            yield (CSharp.Id.Create(t.Id), r)
                    |]
                let extraScope =
                    CSharp.Scope.Build [|
                        for (k, v) in this.AnonymousTypes ->
                            (k, CSharp.ClassNestedInterface v)
                    |]
                CSharp.Scope.Merge extraScope mainScope
            Implements = []
            Inherits = CSharp.Type.Object
            Type = CSharp.Type.Fresh()
        }

    member this.BuildModuleMember(x: R.ModuleMember) : CSharp.Id * CSharp.ClassMember =
        match x with
        | R.ModuleCall s ->
            let n = this.GetName()
            this.BuildMethod(CallStatic n, s, fun ov -> CSharp.ClassMethod(CSharp.Static, ov))
        | R.ModuleMethod (id, s) ->
            let n = this.GetName(id)
            this.BuildMethod(RegularStatic(n), s, fun ov -> CSharp.ClassMethod(CSharp.Static, ov))
        | R.ModuleProperty (id, ty) ->
            this.BuildProperty([Attribute.Stub], id, ty, fun p -> CSharp.ClassProperty(CSharp.Static, p))

    member this.BuildObjectType(defs: seq<S.TypeMember>, prefix: option<string>) : CSharp.Type =
        let defs = R.CompileType defs
        match this.BuildFunType(defs) with
        | Some ft -> ft
        | None -> this.BuildAnonymousType(defs, prefix)

    member this.BuildObjectTypes(defs: seq<S.TypeMember>, prefix: option<string>) : list<CSharp.Type> =
        let defs = R.CompileType defs
        match this.BuildFunTypes(defs) with
        | Some ts -> ts
        | None -> [this.BuildAnonymousType(defs, prefix)]

    member this.BuildProperty<'T>(attr: list<CSharp.Attribute>, id: S.Identifier, ty: CSharp.Type, k: CSharp.Property -> 'T) : CSharp.Id * 'T =
        let r =
            k {
                Attributes = attr
                Type = ty
            }
        (CSharp.Id.Create(id), r)

    member this.BuildProperty<'T>(attr: list<CSharp.Attribute>, id: S.Identifier, ty: S.Type, k: CSharp.Property -> 'T) : CSharp.Id * 'T =
        this.BuildProperty(attr, id, this.BuildType(ty, string id), k)

    member this.BuildSignature(req: seq<S.Parameter>) : CSharp.Signature =
        CSharp.Signature.Create [
            for (S.Parameter (n, t)) in req ->
                CSharp.Parameter.Create(n, this.BuildType t)
        ]

    member this.BuildSignatures(ps: S.Parameters) : list<CSharp.Signature> =
        let variants ps =
            [
                for (S.Parameter (n, t)) in ps ->
                    (n, this.BuildTypes t)
            ]
        let req = variants ps.RequiredParameters
        let opt = variants ps.OptionalParameters
        let makeSignatures =
            match ps.RestParameter with
            | None -> fun ps -> [CSharp.Signature.Create(ps)]
            | Some (S.Parameter (n, t)) ->
                let vs = this.BuildTypes t
                fun ps ->
                    [
                        for v in vs ->
                            CSharp.Signature.Create(ps, CSharp.Parameter.Create(n, v))
                    ]
        inits opt
        |> List.collect (fun opt ->
            req @ opt
            |> explode (fun (n, vs) -> [ for v in vs -> CSharp.Parameter.Create(n, v) ])
            |> List.collect makeSignatures)

    member this.BuildType(t: S.Type, ?prefix: string) : CSharp.Type =
        let rec r (t: S.Type) : CSharp.Type =
            match t with
            | S.AnyType -> CSharp.Type.Object
            | S.ArrayOf x -> CSharp.Type.Array (r x)
            | S.BoolType -> CSharp.Type.Boolean
            | S.NumberType -> CSharp.Type.Number
            | S.ObjectType defs -> this.BuildObjectType(defs, prefix)
            | S.StringType -> CSharp.Type.String
            | S.TypeName name -> this.ResolveType(name)
        r t

    member this.BuildTypes(t: S.Type, ?prefix: string) : list<CSharp.Type> =
        let rec r (t: S.Type) =
            match t with
            | S.AnyType -> [CSharp.Type.Object]
            | S.ArrayOf x -> List.map CSharp.Type.Array (r x)
            | S.BoolType -> [CSharp.Type.Boolean]
            | S.NumberType -> [CSharp.Type.Number]
            | S.ObjectType defs -> this.BuildObjectTypes(defs, prefix)
            | S.StringType -> [CSharp.Type.String]
            | S.TypeName name -> [this.ResolveType(name)]
        r t

    member this.BuildTypeDefinition(t: R.TypeDefinition) : CSharp.ClassMember =
        match t.Kind with
        | R.ClassKind c ->
            let n = this.GetName(t.Id)
            CSharp.ClassNested {
                Attributes = []
                ClassMembers =
                    c.Members
                    |> Seq.map (fun x -> this.BuildClassMember(n, x))
                    |> CSharp.Scope.Build
                Implements =
                    [|
                        for x in c.Implements do
                            match this.Module.Resolve(x) with
                            | None -> ()
                            | Some t -> yield t
                    |]
                Inherits =
                    match c.Inherits with
                    | None -> CSharp.Type.Object
                    | Some t -> this.ResolveType(t)
                Type = t.Type
            }
        | R.EnumKind e ->
            CSharp.ClassNested {
                Attributes = []
                ClassMembers =
                    e
                    |> Seq.map (fun id ->
                        this.BuildProperty([Attribute.Stub], id, t.Type, fun x ->
                            CSharp.ClassProperty(CSharp.Static, x)))
                    |> CSharp.Scope.Build
                Implements = [||]
                Inherits = CSharp.Type.Object
                Type = t.Type
            }
        | R.InterfaceKind i ->
            CSharp.ClassNestedInterface {
                Attributes = []
                Extends = [| for x in i.Extends -> this.ResolveType(x) |]
                InterfaceMembers =
                    i.Members
                    |> Seq.map this.BuildInterfaceMember
                    |> CSharp.Scope.Build
                Type = t.Type
            }

    member this.GetName() =
        match this.QualifiedName with
        | None -> S.Name.Global (S.Identifier "$global")
        | Some n -> n

    member this.GetName(local: S.Identifier) =
        match this.QualifiedName with
        | None -> S.Name.Global local
        | Some n -> n.[local]

    member this.GetNestedContext(id: S.Identifier, m: R.Module) =
        let n = this.GetName(id)
        {
            AnonymousTypes = ResizeArray()
            Module = m
            QualifiedName = Some n
        }

    member this.ResolveType(name: S.Name) =
        match this.Module.Resolve(name) with
        | None -> CSharp.Type.Object
        | Some x -> x

    static member Create(m: R.Module) =
        {
            AnonymousTypes = ResizeArray()
            Module = m
            QualifiedName = None
        }

let Build (ns: seq<string>) (n: string) (file: S.DeclarationSourceFile) : CSharp.Namespace =
    let f = R.Compile file
    let c = Context.Create(f.GlobalModule)
    let m = c.BuildModule()
    {
        Name = CSharp.Name.Create(Seq.head ns, Seq.toArray (Seq.skip 1 ns))
        NamespaceMembers = CSharp.Scope.Build [| (CSharp.Id.Create(n), CSharp.NamespaceClass m) |]
    }
