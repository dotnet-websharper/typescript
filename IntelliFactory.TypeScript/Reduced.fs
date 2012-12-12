module IntelliFactory.TypeScript.Declarations.Reduced

open System.Collections.Generic
module C = IntelliFactory.TypeScript.CSharp
module S = Syntax

[<AutoOpen>]
module Util =

    let ( ++ ) a b =
        Seq.toArray (Seq.append a b) :> seq<_>

    let dictSeq (d: IDictionary<'A,'B>) =
        seq {
            for KeyValue (k, v) in d ->
                (k, v)
        }

type Signature =
    {
        SParameters : S.Parameters
        SReturn : S.Return
    }

    member this.Parameters = this.SParameters
    member this.Return = this.SReturn

    static member Create(p) =
        {
            SParameters = p
            SReturn = S.Void
        }

    static member Create(p, r) =
        {
            SParameters = p
            SReturn = r
        }

type ClassMemberKey =
    | KConstructor
    | KProperty of C.MemberKind * S.Identifier
    | KMethod of C.MemberKind * S.Identifier

type ClassMember =
    | ClassConstructor of seq<S.Parameters>
    | ClassMethod of C.MemberKind * S.Identifier * seq<Signature>
    | ClassProperty of C.MemberKind * S.Identifier * S.Type

    member this.Key =
        match this with
        | ClassConstructor _ -> KConstructor
        | ClassMethod (k, i, _) -> KMethod (k, i)
        | ClassProperty (k, i, _) -> KProperty (k, i)

    static member ( + ) (a: ClassMember, b: ClassMember) =
        match a, b with
        | ClassConstructor x, ClassConstructor y -> ClassConstructor (x ++ y)
        | ClassMethod (k, i, x), ClassMethod (_, _, y) -> ClassMethod (k, i, x ++ y)
        | _, _ -> b

    static member BuildList(x: seq<S.ClassMember>) =
        x
        |> Seq.choose ClassMember.Build
        |> Seq.groupBy (fun x -> x.Key)
        |> Seq.map (fun (_, group) -> group |> Seq.reduce (+))
        |> Seq.toList

    static member MergeLists(x: seq<ClassMember>, y: seq<ClassMember>) =
        Seq.append x y
        |> Seq.groupBy (fun x -> x.Key)
        |> Seq.map (fun (_, group) -> group |> Seq.reduce (+))
        |> Seq.toList

    static member Build(x: S.ClassMember) : option<ClassMember> =
        match x with
        | S.Constructor ps ->
            Some (ClassConstructor [| ps |])
        | S.InstanceField (_, S.Parameter (id, ty)) ->
            Some (ClassProperty (C.Instance, id, ty))
        | S.InstanceFunction (_, S.FunctionSignature (id, _, ps, r)) ->
            Some (ClassMethod (C.Instance, id, [| Signature.Create(ps, r) |]))
        | S.InstanceFunction (_, S.FunctionCallSignature _) ->
            None
        | S.StaticField (S.Parameter (id, ty)) ->
            Some (ClassProperty (C.Static, id, ty))
        | S.StaticFunction (S.FunctionCallSignature _) ->
            None
        | S.StaticFunction (S.FunctionSignature (id, _, ps, r)) ->
            Some (ClassMethod (C.Static, id, [| Signature.Create(ps, r) |]))

type InterfaceMemberKey =
    | ICall
    | IConstruct
    | IIndexer
    | IMethod of S.Identifier
    | IProperty of S.Identifier

type InterfaceMember =
    | InterfaceCall of seq<Signature>
    | InterfaceConstruct of seq<Signature>
    | InterfaceMethod of S.Identifier * seq<Signature>
    | InterfaceIndexer of seq<S.Identifier * S.Type * S.Type>
    | InterfaceProperty of S.Identifier * S.Type

    member this.Key =
        match this with
        | InterfaceCall _ -> ICall
        | InterfaceConstruct _ -> IConstruct
        | InterfaceMethod (id, _) -> IMethod id
        | InterfaceIndexer _ -> IIndexer
        | InterfaceProperty (id, _) -> IProperty id

    static member Build(x: S.TypeMember) : InterfaceMember =
        match x with
        | S.CallMember (S.CallSignature (ps, r))
        | S.FunctionMember (S.FunctionCallSignature (_, ps, r)) ->
            InterfaceCall [| Signature.Create(ps, r) |]
        | S.ConstructMember (S.ConstructSignature (ps, r)) ->
            InterfaceConstruct [| Signature.Create(ps, S.Returns r) |]
        | S.FunctionMember (S.FunctionSignature (id, _, ps, r)) ->
            InterfaceMethod (id, [| Signature.Create(ps, r) |])
        | S.PropertyMember (S.PropertySignature (id, _, ty)) ->
            InterfaceProperty (id, ty)
        | S.IndexMember (S.IndexSignature (S.Parameter (id, tP), ty)) ->
            InterfaceIndexer [| (id, tP, ty) |]

    static member ( + ) (a: InterfaceMember, b: InterfaceMember) =
        match a, b with
        | InterfaceCall x, InterfaceCall y -> InterfaceCall (x ++ y)
        | InterfaceConstruct x, InterfaceConstruct y -> InterfaceConstruct (x ++ y)
        | InterfaceMethod (i, x), InterfaceMethod (_, y) -> InterfaceMethod (i, x ++ y)
        | InterfaceIndexer x, InterfaceIndexer y -> InterfaceIndexer (x ++ y)
        | _, _ -> b

    static member BuildList(x: seq<S.TypeMember>) =
        x
        |> Seq.map InterfaceMember.Build
        |> Seq.groupBy (fun x -> x.Key)
        |> Seq.map (fun (_, group) -> group |> Seq.reduce (+))
        |> Seq.toList

    static member MergeLists(x: seq<InterfaceMember>, y: seq<InterfaceMember>) =
        Seq.append x y
        |> Seq.groupBy (fun x -> x.Key)
        |> Seq.map (fun (_, group) -> group |> Seq.reduce (+))
        |> Seq.toList

let CompileType (ms: seq<S.TypeMember>) =
    InterfaceMember.BuildList(ms)
    |> Seq.ofList

type ModuleMember =
    | ModuleCall of seq<Signature>
    | ModuleMethod of S.Identifier * seq<Signature>
    | ModuleProperty of S.Identifier * S.Type

type Class =
    {
        CImplements : list<S.Name>
        CInherits : option<S.Name>
        CMembers : list<ClassMember>
    }

    member this.Implements = List.toSeq this.CImplements
    member this.Inherits = this.CInherits
    member this.Members = List.toSeq this.CMembers

    static member ( + ) (a: Class, b: Class) =
        {
            CInherits =
                match Option.toList a.Inherits @ Option.toList b.Inherits with
                | [] -> None
                | x :: xs -> Some x
            CImplements = a.CImplements @ b.CImplements
            CMembers = ClassMember.MergeLists(a.CMembers, b.CMembers)
        }

type Interface =
    {
        IExtends : list<S.Name>
        IMembers : list<InterfaceMember>
    }

    member this.Extends = List.toSeq this.IExtends
    member this.Members = List.toSeq this.IMembers

    static member ( + ) (a: Interface, b: Interface) =
        {
            IExtends = a.IExtends @ b.IExtends
            IMembers = InterfaceMember.MergeLists(a.IMembers, b.IMembers)
        }

type Kind =
    | ClassKind of Class
    | EnumKind of seq<S.Identifier>
    | InterfaceKind of Interface

type TypeDefinition =
    {
        DId : S.Identifier
        DKind : Kind
        DType : C.Type
    }

    member this.Id = this.DId
    member this.Kind = this.DKind
    member this.Type = this.DType

    static member Create(c: S.AmbientClassDeclaration) =
        {
            DId = c.Name
            DKind =
                ClassKind {
                    CImplements = Seq.toList c.Implements
                    CInherits = c.Inherits
                    CMembers = ClassMember.BuildList(c.ClassMembers)
                }
            DType = C.Type.Fresh()
        }

    static member Create(S.EnumDeclaration (id, variants)) =
        {
            DId = id
            DKind = EnumKind variants
            DType = C.Type.Fresh()
        }

    static member Create(i: S.InterfaceDeclaration) =
        {
            DId = i.Name
            DKind =
                InterfaceKind {
                    IExtends = Seq.toList i.ExtendedInterfaces
                    IMembers =
                        i.InterfaceMembers
                        |> CompileType
                        |> Seq.toList
                }
            DType = C.Type.Fresh()
        }

type MValue =
    | MField of S.Type
    | MFunction of list<Signature>

type Module =
    {
        GetFile : unit -> DeclarationFile
        GetParent : unit -> option<Module>
        Scope : Scope
    }

    member this.ResolveId(id) =
        match this.Scope.Types.TryGetValue(id) with
        | true, v -> Some v
        | _ ->
            match this.GetParent() with
            | Some m -> m.ResolveId(id)
            | _ -> None

    member this.ResolveModule(id) =
        match this.ResolveId(id) with
        | Some (Module m) -> Some m
        | Some (ImportExternal ext) ->
            match this.GetFile().External.TryGetValue(ext) with
            | true, x -> Some x
            | _ -> None
        | Some (ImportInternal name) ->
            this.ResolveName(name, fun x i -> x.ResolveModule(i))
        | _ -> None

    member this.ResolveName<'X>(name: S.Name, get: Module -> S.Identifier -> option<'X>) : option<'X> =
        match name.Namespace with
        | [] -> get this name.Name
        | x :: xs ->
            match this.ResolveModule(x) with
            | None -> None
            | Some m -> m.ResolveName({ name with Namespace = xs }, get)

    member this.Resolve(name: S.Name) : option<C.Type> =
        this.ResolveName(name, fun x i ->
            match x.ResolveId(i) with
            | Some (Type t) -> Some t.Type
            | _ -> None)

    member this.Members =
        seq {
            for KeyValue (k, v) in this.Scope.Values do
                match k, v with
                | Some id, MField t ->
                    yield ModuleProperty (id, t)
                | Some id, MFunction sigs ->
                    yield ModuleMethod (id, sigs)
                | None, MFunction sigs ->
                    yield ModuleCall sigs
                | _ ->
                    ()
        }

    member this.Modules =
        seq {
            for KeyValue (k, v) in this.Scope.Types do
                match v with
                | Module m -> yield (k, m)
                | _ -> ()
        }

    member this.TypeDefinitions =
        seq {
            for KeyValue (k, v) in this.Scope.Types do
                match v with
                | Type t -> yield t
                | _ -> ()
        }

and Scope =
    {
        Types : IDictionary<S.Identifier,TypeEntity>
        Values : IDictionary<option<S.Identifier>,MValue>
    }

and TypeEntity =
    | ImportExternal of string
    | ImportInternal of S.Name
    | Module of Module
    | Type of TypeDefinition

and DeclarationFile =
    {
        External : IDictionary<string,Module>
        Global : Module
        Merged : Module
    }

    member this.GlobalModule = this.Merged

let mergeMValue (a: MValue) (b: MValue) =
    match a, b with
    | MFunction x, MFunction y -> MFunction (x @ y)
    | _ -> b

let buildValueEnv (s: seq<option<S.Identifier> * MValue>) =
    Seq.groupBy fst s
    |> Seq.map (fun (key, xs) ->
        let ys =
            Seq.map snd xs
            |> Seq.reduce mergeMValue
        (key, ys))
    |> dict

let mergeTD (x: TypeDefinition) (y: TypeDefinition) =
    let k =
        match x.Kind, y.Kind with
        | ClassKind x, ClassKind y -> ClassKind (x + y)
        | EnumKind x, EnumKind y -> EnumKind (x ++ y)
        | InterfaceKind x, InterfaceKind y -> InterfaceKind (x + y)
        | _ -> y.Kind
    { y with DKind = k }

let rec mergeTypeEntity (a: TypeEntity) (b: TypeEntity) =
    match a, b with
    | Module x, Module y -> Module (mergeModule x y)
    | Type x, Type y -> Type (mergeTD x y)
    | _ -> b

and mergeModule (a: Module) (b: Module) =
    {
        GetFile = a.GetFile
        GetParent = b.GetParent
        Scope = mergeScope a.Scope b.Scope
    }

and mergeScope (a: Scope) (b: Scope) =
    {
        Types =
            seq {
                yield! dictSeq a.Types
                yield! dictSeq b.Types
            }
            |> buildTypeEnv
        Values =
            seq {
                yield! dictSeq a.Values
                yield! dictSeq b.Values
            }
            |> buildValueEnv
    }

and buildTypeEnv (s: seq<S.Identifier * TypeEntity>) =
    Seq.groupBy fst s
    |> Seq.map (fun (key, xs) ->
        let ys =
            Seq.map snd xs
            |> Seq.reduce mergeTypeEntity
        (key, ys))
    |> dict

let simplifyInternalModule (m: S.InternalModule) =
    match m.Name.Namespace with
    | [] -> (m.Name.Name, m.Members)
    | n :: ns ->
        let m : S.InternalModule =
            {
                Name = { Namespace = ns; Name = m.Name.Name }
                Members = m.Members
            }
        (n, Seq.singleton (S.ModuleElement m))

let rec compileElementToValue (el: S.ModuleElement) =
    let ret (x: option<S.Identifier>) (v: MValue) = Some (x, v)
    match el with
    | S.FunctionElement (S.FunctionCallSignature (_, ps, r)) ->
        ret None (MFunction [{ SParameters = ps; SReturn = r }])
    | S.FunctionElement (S.FunctionSignature (id, _, ps, r)) ->
        ret (Some id) (MFunction [{ SParameters = ps; SReturn = r }])
    | S.VariableElement (S.Parameter (id, ty)) ->
        ret (Some id) (MField ty)
    | S.ClassElement _
    | S.EnumElement _ 
    | S.ImportElement _
    | S.InterfaceElement _
    | S.ModuleElement _ -> None

let Compile (S.DeclarationSourceFile decls) : DeclarationFile =
    let rec getFile () = file
    and compileElementToType (getParent: unit -> option<Module>) (el: S.ModuleElement) =
        let ret (x: S.Identifier) (e: TypeEntity) = Some (x, e)
        match el with
        | S.ClassElement x ->
            ret x.Name (Type (TypeDefinition.Create x))
        | S.EnumElement x ->
            ret x.Name (Type (TypeDefinition.Create x))
        | S.FunctionElement _ ->
            None
        | S.ImportElement (S.ExternalImport (id, addr)) ->
            ret id (ImportExternal addr)
        | S.ImportElement (S.InternalImport (id, name)) ->
            ret id (ImportInternal name)
        | S.InterfaceElement x ->
            ret x.Name (Type (TypeDefinition.Create x))
        | S.ModuleElement m ->
            let (id, decls) = simplifyInternalModule m
            ret id (Module (compileModule getParent decls))
        | S.VariableElement _ ->
            None
    and compileModule (getParent: unit -> option<Module>) (decls: seq<S.ModuleElement>) : Module =
        let rec self =
            {
                GetFile = getFile
                GetParent = getParent
                Scope =
                    {
                        Types =
                            decls
                            |> Seq.choose (compileElementToType getSelf)
                            |> buildTypeEnv
                        Values =
                            decls
                            |> Seq.choose compileElementToValue
                            |> buildValueEnv
                    }
            }
        and getSelf () = Some self
        self
    and globalModule =
        seq {
            for d in decls do
                match d with
                | S.ExternalModuleDeclaration _ -> ()
                | S.RegularDeclaration el -> yield el
        }
        |> compileModule (fun () -> None)
    and file =
        let getGM () = Some globalModule
        let externals =
            seq {
                for d in decls do
                    match d with
                    | S.ExternalModuleDeclaration em ->
                        yield (em.Path, compileModule getGM em.Members)
                    | S.RegularDeclaration _ -> ()
            }
            |> dict
        let mergedModule : Module =
            let makeID (addr: string) =
                C.Id.Create(addr)
                |> string
                |> S.Identifier
            {
                globalModule with
                    Scope =
                        let extScope : Scope =
                            {
                                Types =
                                    seq {
                                        for KeyValue (k, v) in externals ->
                                            (makeID k, Module v)
                                    }
                                    |> dict
                                Values = dict []
                            }
                        mergeScope extScope globalModule.Scope
            }
        {
            External = externals
            Global = globalModule
            Merged = mergedModule
        }
    file
