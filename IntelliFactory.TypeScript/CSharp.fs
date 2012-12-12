module internal IntelliFactory.TypeScript.CSharp

open System
open System.CodeDom
open System.CodeDom.Compiler
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open Microsoft.CSharp
module S = IntelliFactory.TypeScript.Declarations.Syntax

let provider = CSharpCodeProvider.CreateProvider("C#")

/// Represents valid identifiers.
type Id =
    private { name: string }

    override this.ToString() =
        if provider.IsValidIdentifier(this.name)
            then this.name
            else "@" + this.name

    static member Create(s: string) : Id =
        let clean (s: string) : string =
            match s with
            | null | "" -> "unknown"
            | _ ->
                let s0 =
                    if Char.IsLetter(s.[0]) || s.[0] = '_'
                        then string s.[0]
                        else "_"
                let sR = Regex.Replace(s.Substring(1), @"[^\w]", "_")
                s0 + sR
        { name = clean s }

    static member Create(id: S.Identifier) =
        match id with
        | S.Identifier jN ->
            let n = jN.Replace("$", "_")
            { name = n }

type Name =
    | Global of Id
    | Local of Name * Id

    member this.Id =
        match this with
        | Global x | Local (_, x) -> x

    override this.ToString() =
        use w = new StringWriter()
        let rec f x =
            match x with
            | Global x -> w.Write("{0}", x)
            | Local (a, b) -> f a; w.Write(".{0}", b)
        f this
        w.ToString()

    static member Create(n: string, [<ParamArray>] rest: string []) =
        (Global (Id.Create n), rest)
        ||> Seq.fold (fun x n -> Local (x, Id.Create n))

    member this.Build() : CodeTypeReference =
        CodeTypeReference(string this)

type MemberKind =
    | Instance
    | Static

    member this.Build(x: CodeTypeMember) =
        match this with
        | Static -> x.Attributes <- MemberAttributes.Static ||| MemberAttributes.Public
        | Instance -> x.Attributes <- MemberAttributes.Public

type Scope<'T> =
    private { ScopeMembers: KeyValuePair<Id,'T>[] }

    member private this.Build(scopeName: Id) : IDictionary<Id,'T> =
        let d = Dictionary<Id,'T>()
        let name (prefix: string) (x: int) =
            match x with
            | 0 -> prefix
            | _ -> String.Format("{0}{1:x}", prefix, x)
        let isTaken n =
            scopeName = n || d.ContainsKey(n)
        for KeyValue (k, v) in this.ScopeMembers do
            let rec fresh (n: int) =
                let v = Id.Create(name k.name n)
                if isTaken v then fresh (n + 1) else v
            d.Add(fresh 0, v)
        d :> IDictionary<_,_>

    member this.Iterate(scopeName, add: Id -> 'T -> unit) =
        for KeyValue (k, v) in this.Build(scopeName) do
            add k v

module Scope =

    let Build (xs: seq<Id * 'T>) : Scope<'T> =
        { ScopeMembers = [| for (k, v) in xs -> KeyValuePair(k, v) |] }

    let Map (f: 'T1 -> 'T2) (a: Scope<'T1>) : Scope<'T2> =
        { ScopeMembers = [| for KeyValue (k, v) in a.ScopeMembers -> KeyValuePair(k, f v) |] }

    let Merge (a: Scope<'T>) (b: Scope<'T>) : Scope<'T> =
        { ScopeMembers = Array.append a.ScopeMembers b.ScopeMembers }

[<Sealed>]
type TypeNode private () =
    static let k = ref 0

    let n =
        let v = !k
        incr k
        v

    let mutable name : option<Name> = None

    member this.Name with get () = name and set x = name <- x

    override this.ToString() =
        match name with
        | Some n -> string n
        | _ -> String.Format("type#{0}", n)

    static member Create() = TypeNode()

type Type =
    private
    | ArrayType of Type
    | BooleanType
    | FreshType of TypeNode
    | FunType of Type * Type
    | NamedType of Name
    | NumberType
    | ObjectType
    | StringType
    | TupleType of Type * list<Type>
    | UnitType

    static member Array(t) = ArrayType t
    static member Fresh() = FreshType (TypeNode.Create())
    static member Fun(d, r) = FunType (d, r)
    static member Named(n) = NamedType n

    static member Tuple(x: Type, [<ParamArray>] xs: Type []) =
        TupleType (x, List.ofArray xs)

    static member Boolean = BooleanType
    static member Number = NumberType
    static member Object = ObjectType
    static member String = StringType
    static member Unit = UnitType

    member this.Build() : CodeTypeReference =
        match this with
        | ArrayType t ->
            CodeTypeReference(ArrayRank = 1, ArrayElementType = t.Build())
        | BooleanType -> CodeTypeReference(typeof<bool>)
        | FreshType n ->
            match n.Name with
            | Some n -> n.Build()
            | None -> CodeTypeReference(typeof<obj>)
        | FunType (d, r) ->
            CodeTypeReference(typedefof<_->_>.FullName, d.Build(), r.Build())
        | NamedType n -> n.Build()
        | NumberType -> CodeTypeReference("IntelliFactory.WebSharper.Number")
        | ObjectType -> CodeTypeReference(typeof<obj>)
        | StringType -> CodeTypeReference(typeof<string>)
        | TupleType (t, ts) ->
            CodeTypeReference("System.Tuple",
                [| for x in t :: ts -> x.Build () |])
        | UnitType -> CodeTypeReference(typeof<unit>)

[<Sealed>]
type Parameter(name: Id, ty: Type) =

    override this.Equals(other: obj) =
        match other with
        | :? Parameter as o -> this.Type = o.Type
        | _ -> false

    override this.GetHashCode() = hash ty

    member this.Id = name
    member this.Type = ty

    static member Create(id: S.Identifier, t: Type) = Parameter(Id.Create(id), t)
    static member Create(id: Id, t: Type) = Parameter(id, t)

    member this.Build() =
        CodeParameterDeclarationExpression(ty.Build(), string name)

type Signature =
    {
        Parameters : list<Parameter>
        RestParameter : option<Parameter>
    }

    member this.Arity =
        this.Parameters.Length +
            match this.RestParameter with
            | None -> 0
            | Some _ -> 1

    static member Create(ps) =
        {
            Parameters = ps
            RestParameter = None
        }

    static member Create(ps, rest) =
        {
            Parameters = ps
            RestParameter = Some rest
        }

    member this.Build(c: CodeParameterDeclarationExpressionCollection) =
        for p in this.Parameters do
            c.Add(p.Build()) |> ignore
        match this.RestParameter with
        | None -> ()
        | Some p ->
            let x = p.Build()
            let a = CodeAttributeDeclaration(CodeTypeReference(typeof<ParamArrayAttribute>))
            x.CustomAttributes.Add(a) |> ignore
            c.Add(x) |> ignore

type Attribute =
    {
        AttributeArguments : list<string>
        AttributeType : Type
    }

    member this.Build(c: CodeAttributeDeclarationCollection) =
        let t = this.AttributeType.Build()
        let a =
            [|
                for a in this.AttributeArguments ->
                    let x = CodePrimitiveExpression(a)
                    CodeAttributeArgument(x)
            |]
        CodeAttributeDeclaration(t, a)
        |> c.Add
        |> ignore

    static member Create(t: Type, [<ParamArray>] xs: string []) =
        {
            AttributeArguments = List.ofArray xs
            AttributeType = t
        }

[<Sealed>]
type Overloads private (variants: IDictionary<Signature,option<Type>*list<Attribute>>) =

    member this.Build(k: Signature -> option<Type> -> list<Attribute> -> unit) =
        for KeyValue (s, (t, a)) in variants do
            k s t a

    static member Build(variants: seq<Signature * option<Type> * list<Attribute>>) : Overloads =
        Overloads (dict (seq { for (s, t, a) in variants -> (s, (t, a)) }))

type Property =
    {
        Attributes : list<Attribute>
        Type : Type
    }

    member this.Build(id: Id) =
        let p = CodeMemberProperty(Name = string id)
        p.HasGet <- true
        p.HasSet <- true
        p.Type <- this.Type.Build()
        for a in this.Attributes do
            a.Build(p.CustomAttributes)
        p

type IndexSignature =
    {
        Attributes : list<Attribute>
        Parameter : Parameter
        Type : Type
    }

[<Sealed>]
type IndexerOverloads private (variants: IDictionary<Type,Id * Type * list<Attribute>>) =

    static member Create(xs: seq<IndexSignature>) =
        let d =
            seq {
                for x in xs ->
                    (x.Parameter.Type, (x.Parameter.Id, x.Type, x.Attributes))
            }
            |> dict
        IndexerOverloads(d)

    member this.Iterate(k) =
        for KeyValue (pT, (pN, rT, atts)) in variants do
            k pN pT rT atts

type Interface =
    {
        Attributes : list<Attribute>
        Extends : seq<Type>
        InterfaceMembers : Scope<InterfaceMember>
        Type : Type
    }

    member this.Build(id: Id) =
        let self = CodeTypeDeclaration(string id, IsInterface=true)
        this.InterfaceMembers.Iterate(id, fun id m -> m.Build(self, id))
        let baseTypes =
            seq {
                for i in this.Extends do
                    match i with
                    | FreshType n when n.Name.IsSome ->
                        yield NamedType n.Name.Value
                    | NamedType _ ->
                        yield i
                    | _ -> ()
            }
            |> Seq.distinct
            |> Seq.iter (fun t -> self.BaseTypes.Add(t.Build()) |> ignore)
        self

    member this.Build(n: CodeNamespace, id: Id) =
        n.Types.Add(this.Build(id)) |> ignore

and InterfaceMember =
    | InterfaceIndex of IndexerOverloads
    | InterfaceMethod of Overloads
    | InterfaceProperty of Property

    member this.Build(self: CodeTypeDeclaration, id: Id) =
        match this with
        | InterfaceIndex overloads ->
            overloads.Iterate(fun id t1 t2 atts ->
                let p : Property =
                    {
                        Attributes = atts
                        Type = t2
                    }
                let p = p.Build(Id.Create("Item"))
                Parameter.Create(id, t1).Build()
                |> p.Parameters.Add
                |> ignore
                self.Members.Add(p)
                |> ignore)
        | InterfaceMethod overloads ->
            overloads.Build(fun s r a ->
                let m = CodeMemberMethod(Name = string id)
                s.Build(m.Parameters)
                for x in a do
                    x.Build(m.CustomAttributes)
                match r with
                | Some t -> m.ReturnType <- t.Build()
                | None -> m.ReturnType <- CodeTypeReference(typeof<Void>)
                self.Members.Add(m) |> ignore)
        | InterfaceProperty p ->
            let p = p.Build(id)
            self.Members.Add(p) |> ignore

let throwException =
    new CodeThrowExceptionStatement(CodePrimitiveExpression(null))

type Class =
    {
        Attributes : list<Attribute>
        ClassMembers : Scope<ClassMember>
        Implements : seq<Type>
        Inherits : Type
        Type : Type
    }

    member this.Build(id: Id) =
        let self = CodeTypeDeclaration(string id, IsClass=true)
        let hasCtors = ref false
        this.ClassMembers.Iterate(id, fun id m ->
            match m with
            | ClassConstructor _ -> hasCtors := true
            | _ -> ()
            m.Build(self, id))
        if not !hasCtors then
            self.Members.Add(CodeConstructor()) |> ignore
        let baseTypes =
            seq {
                match this.Inherits with
                | ObjectType -> ()
                | t -> yield t
                for i in this.Implements do
                    match i with
                    | FreshType n when n.Name.IsSome ->
                        yield NamedType n.Name.Value
                    | NamedType _ ->
                        yield i
                    | _ -> ()
            }
            |> Seq.distinct
            |> Seq.iter (fun t -> self.BaseTypes.Add(t.Build()) |> ignore)
        self

    member this.Build(n: CodeNamespace, id: Id) =
        n.Types.Add(this.Build(id)) |> ignore

and ClassMember =
    | ClassConstructor of Overloads
    | ClassMethod of MemberKind * Overloads
    | ClassNested of Class
    | ClassNestedInterface of Interface
    | ClassProperty of MemberKind * Property

    member this.Build(self: CodeTypeDeclaration, id: Id) =
        match this with
        | ClassConstructor overloads ->
            overloads.Build(fun s r a ->
                let c = CodeConstructor()
                c.Attributes <- MemberAttributes.Public
                s.Build(c.Parameters)
                for x in a do
                    x.Build(c.CustomAttributes)
                self.Members.Add(c) |> ignore)
        | ClassMethod (kind, overloads) ->
            overloads.Build(fun s r a ->
                let m = CodeMemberMethod(Name = string id)
                kind.Build(m)
                s.Build(m.Parameters)
                for x in a do
                    x.Build(m.CustomAttributes)
                match r with
                | Some t ->
                    m.Statements.Add(throwException) |> ignore
                    m.ReturnType <- t.Build()
                | None -> m.ReturnType <- CodeTypeReference(typeof<Void>)
                self.Members.Add(m) |> ignore)
        | ClassNested c ->
            self.Members.Add(c.Build(id)) |> ignore
        | ClassNestedInterface i ->
            self.Members.Add(i.Build(id)) |> ignore
        | ClassProperty (kind, p) ->
            let p = p.Build(id)
            kind.Build(p)
            p.GetStatements.Add(throwException) |> ignore
            self.Members.Add(p) |> ignore

type Namespace =
    {
        Name : Name
        NamespaceMembers : Scope<NamespaceMember>
    }

    member this.Build() : CodeNamespace =
        let ns = CodeDom.CodeNamespace(string this.Name)
        this.NamespaceMembers.Iterate(this.Name.Id, fun id x ->
            x.Build(ns, id))
        ns

and NamespaceMember =
    | NamespaceClass of Class
    | NamespaceInterface of Interface

    member this.Build(n: CodeNamespace, id: Id) =
        match this with
        | NamespaceClass x -> x.Build(n, id)
        | NamespaceInterface x -> x.Build(n, id)

let BuildTypes (ns: Namespace) =
    let visitType (n: Name) (t: Type) =
        match t with
        | FreshType r ->
            match r.Name with
            | None -> r.Name <- Some n
            | _ -> ()
        | _ -> ()
    let rec visitClass (n: Name) (c: Class) =
        visitType n c.Type
        c.ClassMembers.Iterate(n.Id, fun k v ->
            match v with
            | ClassNested nC -> visitClass (Local (n, k)) nC
            | ClassNestedInterface i -> visitInterface (Local (n, k)) i
            | _ -> ())
    and visitInterface (n: Name) (i: Interface) =
        visitType n i.Type
    ns.NamespaceMembers.Iterate(ns.Name.Id, fun k v ->
        let n = Local (ns.Name, k)
        match v with
        | NamespaceClass c -> visitClass n c
        | NamespaceInterface i -> visitInterface n i)

let Compile (out: TextWriter) (ns: Namespace) =
    BuildTypes ns
    let opts = CodeGeneratorOptions()
    let u = CodeCompileUnit()
    u.Namespaces.Add(ns.Build()) |> ignore
    opts.BracingStyle <- "C"
    opts.IndentString <- "  "
    provider.GenerateCodeFromCompileUnit(u, out, opts)
