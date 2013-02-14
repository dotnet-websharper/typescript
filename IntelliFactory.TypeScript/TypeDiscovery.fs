module internal IntelliFactory.TypeScript.TypeDiscovery

open System.Collections.Generic
module A = Assembling
module C = Contexts
module S = Syntax
module SR = SyntaxReductions

type Module =
    {
        Location : C.Location
        Methods : seq<S.FunctionSignature>
        Properties : seq<S.Parameter>
        Types : seq<S.Identifier>
    }

    static member Create(loc: C.Location)(elements: seq<S.ModuleElement>) : Module =
        let elements = Seq.toArray elements
        let properties =
            elements
            |> Array.choose (function
                | S.VariableElement p -> Some p
                | _ -> None)
        let methods =
            elements
            |> Array.choose (function
                | S.FunctionElement p -> Some p
                | _ -> None)
        let types =
            elements
            |> Array.choose (function
                | S.ClassElement { Name = x }
                | S.EnumElement { Name = x } ->
                    Some x
                | S.ModuleElement { Name = x } ->
                    Some x.Head
                | _ -> None)
        {
            Location = loc
            Methods = methods
            Properties = properties
            Types = types
        }

type Class =
    {
        Implements : seq<S.Name>
        Inherits : option<S.Name>
        Location : C.Location
        Members : seq<S.ClassMember>
    }

    static member Create(loc: C.Location)(decl: S.AmbientClassDeclaration) =
        {
            Implements = decl.Implements
            Inherits = decl.Inherits
            Location = loc
            Members = decl.ClassMembers
        }

type Interface =
    {
        Extends : seq<S.Name>
        Location : C.Location
        Members : seq<S.TypeMember>
    }

    static member Create(loc: C.Location)(b: S.InterfaceDeclaration) =
        {
            Extends = b.ExtendedInterfaces
            Location = loc
            Members = b.InterfaceMembers
        }

type Enum =
    {
        Location : C.Location
        Variants : seq<S.Identifier>
    }

    static member Create(loc: C.Location)(decl: S.EnumDeclaration) =
        {
            Location = loc
            Variants = decl.Variants
        }

type DiscoveredType =
    | DiscoveredClass of Class
    | DiscoveredEnum of Enum
    | DiscoveredInterface of Interface
    | DiscoveredModule of Module

    member this.Location =
        match this with
        | DiscoveredClass x -> x.Location
        | DiscoveredEnum x -> x.Location
        | DiscoveredInterface x -> x.Location
        | DiscoveredModule x -> x.Location

type EntityKey =
    | GlobalLike of C.Context * A.Key
    | TypeLike of C.Location

type DiscoveredEntity =
    | DiscoveredGlobal of C.Context * SR.Node
    | DiscoveredImportInternal of C.Location * C.Context * S.Name
    | DiscoveredImportExternal of C.Location * C.Path
    | DiscoveredTypeEntity of DiscoveredType

    member this.Key =
        match this with
        | DiscoveredGlobal (c, SR.Node (key, _)) ->
            GlobalLike (c, key)
        | DiscoveredImportInternal (loc, _, _)
        | DiscoveredImportExternal (loc, _) ->
            TypeLike loc
        | DiscoveredTypeEntity dt ->
            TypeLike dt.Location

let NormalizeModuleEntry (m: S.InternalModule) =
    let rec norm (m: S.InternalModule) =
        match m.Name with
        | S.GlobalName id -> (id, m.Members)
        | S.LocalName (parent, local) ->
            norm {
                Name = parent
                Members = [S.ModuleElement { Name = S.Name.Global local; Members = m.Members }]
            }
    norm m

let FindTypes (file: S.DeclarationSourceFile) : seq<DiscoveredEntity> =
    let rec inME (ctx: C.Context) (mdl: S.ModuleElement) =
        match mdl with
        | S.ClassElement c ->
            let loc = ctx.RelativeLocation(c.Name)
            let cl = Class.Create loc c
            Seq.singleton (DiscoveredTypeEntity (DiscoveredClass cl))
        | S.EnumElement e ->
            let loc = ctx.RelativeLocation(e.Name)
            let en = Enum.Create loc e
            Seq.singleton (DiscoveredTypeEntity (DiscoveredEnum en))
        | S.FunctionElement fs ->
            let node = SR.FunctionNode fs
            Seq.singleton (DiscoveredGlobal (ctx, node))
        | S.ImportElement (S.ImportDeclaration.ExternalImport (id, path)) ->
            let loc = ctx.RelativeLocation(id)
            Seq.singleton (DiscoveredImportExternal (loc, path))
        | S.ImportElement (S.ImportDeclaration.InternalImport (id, name)) ->
            let loc = ctx.RelativeLocation(id)
            Seq.singleton (DiscoveredImportInternal (loc, ctx, name))
        | S.InterfaceElement i ->
            let loc = ctx.RelativeLocation(i.Name)
            let int = Interface.Create loc i
            Seq.singleton (DiscoveredTypeEntity (DiscoveredInterface int))
        | S.ModuleElement m ->
            let (id, ms) = NormalizeModuleEntry m
            let loc = ctx.RelativeLocation(id)
            let ctx = ctx.RelativeContext(id)
            let mdl = Module.Create loc m.Members
            seq {
                yield DiscoveredTypeEntity (DiscoveredModule mdl)
                for x in m.Members do
                    yield! inME ctx x
            }
        | S.VariableElement par ->
            Seq.singleton (DiscoveredGlobal (ctx, SR.ParameterNode par))
    seq {
        match file with
        | S.DeclarationSourceFile decls ->
            for d in decls do
                match d with
                | S.ExternalModuleDeclaration m ->
                    let loc = C.Location.External m.Path
                    let ctx = C.Context.External m.Path
                    let mdl = Module.Create loc m.Members
                    yield DiscoveredTypeEntity (DiscoveredModule mdl)
                    for me in m.Members do
                        yield! inME ctx me
                | S.RegularDeclaration me ->
                    yield! inME C.Context.Global me
    }

let ReduceDiscoveredType (log: Logging.Log) (a: DiscoveredType) (b: DiscoveredType) : DiscoveredType =
    match a, b with
    | DiscoveredInterface x, DiscoveredInterface y ->
        DiscoveredInterface {
            Location = y.Location
            Extends = Seq.append x.Extends y.Extends
            Members = Seq.append x.Members y.Members
        }
    | DiscoveredModule x, DiscoveredModule y ->
        DiscoveredModule {
            Location = x.Location
            Methods = Seq.append x.Methods y.Methods
            Properties = Seq.append x.Properties y.Properties
            Types = Seq.append x.Types y.Types
        }
    | _ ->
        log.Warning("Name conflict on {0}", a.Location)
        b

let ReduceDiscoveredEntity (log: Logging.Log) (a: DiscoveredEntity) (b: DiscoveredEntity) =
    match a, b with
    | DiscoveredTypeEntity a, DiscoveredTypeEntity b ->
        DiscoveredTypeEntity (ReduceDiscoveredType log a b)
    | DiscoveredGlobal (ctx, SR.Node (id, t1)), DiscoveredGlobal (_, SR.Node (_, t2)) ->
        DiscoveredGlobal (ctx, SR.Node (id, SR.ReduceTypes t1 t2))
    | _ ->
        log.Warning("Name conflict on {0}", a.Key)
        b

/// Merges entities by grouping by location.
let MergeEntities (log: Logging.Log) (input: seq<DiscoveredEntity>) : seq<DiscoveredEntity> =
    input
    |> Seq.MergeDuplicates
        (fun x -> x.Key)
        HashIdentity.Reference
        (ReduceDiscoveredEntity log)

type Result = seq<DiscoveredEntity>

let Discover (log: Logging.Log) (input: S.DeclarationSourceFile) : Result =
    log.Information "Discovery"
    FindTypes input
    |> MergeEntities log
