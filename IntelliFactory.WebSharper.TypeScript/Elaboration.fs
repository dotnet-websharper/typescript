module internal IntelliFactory.TypeScript.Elaboration

open System
open System.Collections.Generic
module M = Mappings
module S = Syntax

type Scope =
    | ExternScope of S.Path
    | GlobalScope

    override this.ToString() =
        match this with
        | ExternScope p -> String.Format("[{0}]", p)
        | GlobalScope -> "global"

type Location =
    | Extern of S.Path
    | Local of Scope * S.Name

    override this.ToString() =
        match this with
        | Extern x -> string x
        | Local (scope, name) -> String.Format("{0}:{1}", scope, name)

type Context =
    | At of Location
    | Global

    override this.ToString() =
        match this with
        | At x -> string x
        | Global -> "global"

    member this.Item
        with get (id: S.Identifier) : Location =
            match this with
            | At (Local (scope, name)) -> Local (scope, name.[id])
            | At (Extern p) -> Local (ExternScope p, S.Name.Global id)
            | Global -> Local (GlobalScope, S.Name.Global id)

type Overload =
    {
        Signature : Signature
        Return : Type
    }

and Overloads =
    private {
        OverloadVariants : list<Overload>
    }

    member this.IsEmpty =
        this.OverloadVariants.IsEmpty

    member this.Variants =
        this.OverloadVariants

    static member Create(variants: seq<Overload>) : Overloads =
        let vs =
            variants
            |> Seq.distinctBy (fun v -> v.Signature)
            |> Seq.toList
        { OverloadVariants = vs }

    static member Empty : Overloads =
        { OverloadVariants = [] }

    static member Union(overloads: seq<Overloads>) : Overloads =
        overloads
        |> Seq.collect (fun x -> x.OverloadVariants)
        |> Overloads.Create

and Property =
    | Optional of Type
    | Required of Type

    member this.Type =
        match this with
        | Optional t
        | Required t -> t

    static member Union(ps: seq<Property>) =
        let isRequired =
            ps
            |> Seq.exists (function
                | Required _ -> true
                | _ -> false)
        let mk t = if isRequired then Required t else Optional t
        let ts = ps |> Seq.map (fun x -> x.Type) |> Seq.toList
        match ts with
        | [] -> Optional Type.AnyType
        | _ ->
            let nonStruct =
                ts
                |> Seq.tryPick (function
                    | StructType _ -> None
                    | o -> Some o)
            match nonStruct with
            | Some t -> mk t
            | None ->
                ts
                |> Seq.map (function StructType s -> s | _ -> failwith "impossible")
                |> Struct.Union
                |> StructType
                |> mk

and Signature =
    Signatures.Signature<Type>

and Struct =
    {
        Call : Overloads
        Construct : Overloads
        IndexByNumber : option<Type>
        IndexByString : option<Type>
        Properties : M.Mapping<S.Identifier,Property>
    }

    static member Empty : Struct =
        {
            Call = Overloads.Empty
            Construct = Overloads.Empty
            IndexByNumber = None
            IndexByString = None
            Properties = M.Empty
        }

    static member Singleton(id: S.Identifier, p: Property) : Struct =
        { Struct.Empty with Properties = M.Singleton id p }

    static member Union(ss: seq<Struct>) : Struct =
        {
            Call = ss |> Seq.map (fun x -> x.Call) |> Overloads.Union
            Construct = ss |> Seq.map (fun x -> x.Construct) |> Overloads.Union
            IndexByNumber = ss |> Seq.tryPick (fun x -> x.IndexByNumber)
            IndexByString = ss |> Seq.tryPick (fun x -> x.IndexByString)
            Properties =
                ss
                |> Seq.map (fun x -> x.Properties)
                |> M.UnionWith Property.Union
        }

and Type =
    | AnyType
    | ArrayType of Type
    | BooleanType
    | LocatedType of Location
    | NamedType of Context * S.Name
    | NumberType
    | StringType
    | StructType of Struct
    | UnitType

/// View deeply nested syntactic modules as shallowly nested.
let private (|InternalModule|) (m: S.InternalModule) : S.Identifier * seq<S.ModuleElement> =
    let rec norm (m: S.InternalModule) =
        match m.Name with
        | Symbols.GlobalName id -> (id, m.Members)
        | Symbols.LocalName (parent, local) ->
            norm {
                Name = parent
                Members = [S.ModuleElement { Name = S.Name.Global local; Members = m.Members }]
            }
    norm m

type Fact =
    | ClassInherits of Location * Context * S.Name
    | ClassStaticsInclude of Location * Struct
    | ContractIncludes of Context * Struct
    | ImportExternal of S.Identifier * Context * S.Path
    | ImportInternal of S.Identifier * Context * S.Name
    | InterfaceExtends of Location * Context * S.Name
    | IsClass of Location
    | IsEnum of Location
    | IsInterface of Location
    | IsModule of Location
    | PathIsDefined of S.Path

[<Sealed>]
type Elaborator private (trace: Logging.Log, ctx: Context, fact: Fact -> unit) =

    member private this.ClassMember(loc: Location, m: S.ClassMember) : unit =
        match m with
        | S.Constructor ps ->
            let r = LocatedType loc
            fact <| ClassStaticsInclude (loc, this.StructConstructSignature(ps, r))
        | S.InstanceFunction (vis, f) ->
            match vis with
            | S.Private -> ()
            | _ ->
                let f = this.StructFunctionSignature(f)
                fact <| ContractIncludes (At loc, f)
        | S.InstanceField (vis, p) ->
            match vis with
            | S.Private -> ()
            | _ ->
                let p = this.StructPropertySignature(p)
                fact <| ContractIncludes(At loc, p)
        | S.StaticField p ->
            fact <| ClassStaticsInclude(loc, this.StructPropertySignature(p))
        | S.StaticFunction f ->
            fact <| ClassStaticsInclude(loc, this.StructFunctionSignature(f))

    member private this.ModuleElement(e: S.ModuleElement) : unit =
        match e with
        | S.CallElement c ->
            fact <| ContractIncludes(ctx, this.StructCallSignature(c))
        | S.ClassElement c ->
            let loc = ctx.[c.Name]
            fact <| IsClass(loc)
            for m in c.ClassMembers do
                this.ClassMember(loc, m)
            match c.Inherits with
            | Some n -> fact <| ClassInherits(loc, ctx, n)
            | _ -> ()
        | S.EnumElement e ->
            let loc = ctx.[e.Name]
            fact <| IsEnum(loc)
            let t = LocatedType loc
            for v in e.Variants do
                fact <| ClassStaticsInclude(loc, this.StructPropertySignature(v, t))
        | S.FunctionElement f ->
            fact <| ContractIncludes(ctx, this.StructFunctionSignature(f))
        | S.ImportElement decl ->
            match decl with
            | S.ExternalImport (id, path) ->
                fact <| ImportExternal (id, ctx, path)
            | S.InternalImport (id, name) ->
                fact <| ImportInternal (id, ctx, name)
        | S.InterfaceElement i ->
            let loc = ctx.[i.Name]
            fact <| IsInterface(loc)
            let c = At loc
            for ext in i.ExtendedInterfaces do
                fact <| InterfaceExtends(loc, ctx, ext)
            for m in i.InterfaceMembers do
                fact <| ContractIncludes(c, this.TypeMember(m))
        | S.ModuleElement (InternalModule (id, ms)) ->
            let loc = ctx.[id]
            let elab = Elaborator(trace, At loc, fact)
            fact <| IsModule(loc)
            for e in ms do
                elab.ModuleElement(e)
        | S.VariableElement p ->
            let t = this.StructPropertySignature(p)
            fact <| ContractIncludes(ctx, t)

    member private this.Signatures(ps: S.Parameters) : list<Signatures.Signature<Type>> =
        let par (S.Parameter (_, t)) =
            this.Type(t)
        let rec inits (el: list<'T>) : list<list<'T>> =
            match el with
            | [] -> [[]]
            | x :: xs -> [] :: [for rest in inits xs -> x :: rest]
        let req = Seq.map par ps.RequiredParameters |> Seq.toList
        let opt = Seq.map par ps.OptionalParameters |> Seq.toList
        let rest =
            match ps.RestParameter with
            | None -> Signatures.Empty
            | Some (S.Parameter (_, t)) -> Signatures.Rest (this.Type(t))
        let cons x xs = Signatures.With(x, xs)
        [
            for opt in inits opt ->
                rest
                |> List.foldBack cons opt
                |> List.foldBack cons req
        ]

    member private this.StructCallSignature(S.CallSignature (par, ret)) : Struct =
        let ov =
            let r =
                match ret with
                | S.Returns t -> this.Type(t)
                | S.Void -> UnitType
            seq { for s in this.Signatures(par) -> { Signature = s; Return = r } }
            |> Overloads.Create
        { Struct.Empty with Call = ov }

    member private this.StructConstructSignature(S.ConstructSignature (par, t)) : Struct =
        this.StructConstructSignature(par, this.Type(t))

    member private this.StructConstructSignature(par: S.Parameters, r: Type) : Struct =
        let ov =
            seq { for s in this.Signatures(par) -> { Signature = s; Return = r } }
            |> Overloads.Create
        { Struct.Empty with Construct = ov }

    member private this.StructFunctionSignature(f: S.FunctionSignature) : Struct =
        match f with
        | S.FunctionSignature (id, req, par, ret) ->
            let c =
                this.StructCallSignature (S.CallSignature (par, ret))
                |> StructType
            let prop =
                match req with
                | S.Optional -> Optional c
                | S.Required -> Required c
            Struct.Singleton (id, prop)

    member private this.StructIndexSignature(S.IndexSignature (S.Parameter (_, ty), r)) : Struct =
        let t = this.Type(ty)
        match ty with
        | S.StringType -> { Struct.Empty with IndexByString = Some t }
        | S.NumberType -> { Struct.Empty with IndexByNumber = Some t }
        | _ -> Struct.Empty

    member private this.StructPropertySignature(S.PropertySignature (id, req, ty) : S.PropertySignature) : Struct =
        let t = this.Type(ty)
        let prop =
            match req with
            | S.Optional -> Optional t
            | S.Required -> Required t
        Struct.Singleton (id, prop)

    member private this.StructPropertySignature(S.Parameter (id, ty) : S.Parameter) : Struct =
        Struct.Singleton (id, Required (this.Type(ty)))

    member private this.StructPropertySignature(id: S.Identifier, t: Type) : Struct =
        Struct.Singleton (id, Required t)

    member private this.Type(t: S.Type) : Type =
        match t with
        | S.AnyType -> AnyType
        | S.ArrayOf eT -> ArrayType (this.Type eT)
        | S.BoolType -> BooleanType
        | S.NumberType -> NumberType
        | S.ObjectType ms -> StructType (this.TypeMembers ms)
        | S.StringType -> StringType
        | S.TypeName name -> NamedType (ctx, name)

    member private this.TypeMember(m: S.TypeMember) : Struct =
        match m with
        | S.CallMember s -> this.StructCallSignature(s)
        | S.ConstructMember c -> this.StructConstructSignature(c)
        | S.FunctionMember f -> this.StructFunctionSignature(f)
        | S.IndexMember i -> this.StructIndexSignature(i)
        | S.PropertyMember p -> this.StructPropertySignature(p)

    member private this.TypeMembers(ms: seq<S.TypeMember>) : Struct =
        Struct.Union (Seq.map this.TypeMember ms)

    static member DeclarationSourceFile(trace: Logging.Log, fact: Fact -> unit, S.DeclarationSourceFile decls) : unit =
        let globalElab = Elaborator(trace, Global, fact)
        for d in decls do
            match d with
            | S.ExternalModuleDeclaration em ->
                let elab = Elaborator(trace, At (Extern em.Path), fact)
                fact <| PathIsDefined em.Path
                for e in em.Members do
                    elab.ModuleElement(e)
            | S.RegularDeclaration e ->
                globalElab.ModuleElement(e)

let Elaborate (trace: Logging.Log) (file: S.DeclarationSourceFile) : seq<Fact> =
    let q = Queue()
    let fact e = q.Enqueue(e)
    Elaborator.DeclarationSourceFile(trace, fact, file)
    q.ToArray() :> seq<_>
