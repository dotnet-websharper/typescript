/// Builds INameResolver by constructing a name trie.
module internal IntelliFactory.TypeScript.NameResolution

module C = Contexts
module D = TypeDiscovery
module M = Memoization
module S = Syntax

type Resolution = option<D.DiscoveredType>

type INameResolver =
    abstract ResolveName : C.Context * S.Name -> Resolution
    abstract ResolveLocation : C.Location -> Resolution

[<Sealed>]
type private NameResolver(resolveLocation: C.Location -> Resolution) =
    let locationTable = M.Memoize (M.Options()) resolveLocation
    interface INameResolver with
        member this.ResolveLocation(loc) = locationTable.[loc]
        member this.ResolveName(ctx: C.Context, name: S.Name) =
            ctx.AncestorsAndSelf()
            |> Seq.tryPick (fun ctx -> locationTable.[ctx.[name]])

[<Sealed>]
type private RootNode(log: Logging.Log, pathTable: Mapping<string,Node>, entryTable: Mapping<S.Identifier,Entry>) =

    /// Usese the path table to lookup a node.
    let FindPath (path: string) : option<Node> =
        pathTable.TryFind(path)

    /// Usese the entry table to look up an entry.
    let FindId (id: S.Identifier) : option<Entry> =
        entryTable.TryFind(id)

    /// Searching for X.Y.Z - recurse on the structure, trie-like.
    let rec SearchAt(loc: C.Location) : option<Entry> =
        match loc with
        | { Scope = C.Global; Name = Symbols.GlobalName id } ->
            FindId(id)
        | { Scope = s; Name = Symbols.LocalName (name, local) } ->
            SearchAt { Scope = s; Name = name }
            |> Option.bind (SearchFor(local))
        | { Scope = C.External p; Name = Symbols.GlobalName id } ->
            match FindPath(p) with
            | None -> None
            | Some node -> node.FindId(id)
//        | C.Extern path ->
//            FindPath(path)
//            |> Option.map NodeEntry

    /// Searching for X in an entry - import/symlink resolution happens here.
    and SearchFor (id: S.Identifier) (result: Entry) : option<Entry> =
        match result with
        | NodeEntry node ->
            node.FindId(id)
        | InternalImportEntry (ctx, name) ->
            FindName(ctx)(name)
            |> Option.bind (SearchFor(id))
        | ExternalImportEntry path ->
            FindPath(path)
            |> Option.bind (NodeEntry >> SearchFor id)

    /// Searching in X.Y.Z for A.B.C is equivalent to finding the first pick
    /// of X.Y.Z.A.B.C, X.Y.A.B.C, X.A.B.C, A.B.C.
    and FindName(ctx: C.Context)(name: S.Name) : option<Entry> =
        ctx.AncestorsAndSelf()
        |> Seq.tryPick (fun x -> SearchAt(x.[name]))

    member this.Resolve(loc) =
        match SearchAt loc with
        | Some (NodeEntry node) -> Some node.Type
        | _ -> None

and [<Sealed>] private Node(table: Mapping<S.Identifier,Entry>, t: D.DiscoveredType) =
    member this.FindId(id: S.Identifier) : option<Entry> = table.TryFind(id)
    member this.Type = t

and private Entry =
    | ExternalImportEntry of string
    | InternalImportEntry of C.Context * S.Name
    | NodeEntry of Node

let ConstructResolver (log: Logging.Log) (input: seq<D.DiscoveredEntity>) : INameResolver =
    let (|Nested|_|) (c: C.Location) =
        match c with
        | { Scope = C.Global; Name = Symbols.GlobalName id } ->
            Some (C.In C.Global, id)
        | { Scope = C.Global; Name = Symbols.LocalName (name, local) } ->
            Some (C.At { Scope = C.Global; Name = name }, local)
        | _ ->
            None
    let byContext =
        input
        |> Seq.choose (fun ent ->
            match ent.Key with
            | D.TypeLike (Nested (ctx, id)) -> Some (ctx, (id, ent))
            | _ -> None)
        |> Seq.groupBy fst
        |> Seq.map (fun (ctx, rest) ->
            let entries =
                rest
                |> Seq.map snd
                |> Mapping.New
            (ctx, entries))
        |> Mapping.New
    let rec buildEntryTable (ctx: C.Context) =
        match byContext.TryFind(ctx) with
        | None -> Mapping()
        | Some xs ->
            xs
            |> Mapping.Choose (fun (id: S.Identifier) entity ->
                match entity with
                | D.DiscoveredTypeEntity entity ->
                    let nestedContext = C.At (ctx.[id])
                    Node(buildEntryTable nestedContext, entity)
                    |> NodeEntry
                    |> Some
                | D.DiscoveredImportExternal (loc, path) ->
                    Some (ExternalImportEntry path)
                | D.DiscoveredImportInternal (loc, ctx, name) ->
                    Some (InternalImportEntry (ctx, name))
                | D.DiscoveredGlobal _ ->
                    None)

    let entryTable = buildEntryTable (C.In C.Global)
    let pathTable =
        Seq.empty
//        input
//        |> Seq.choose (fun v ->
//            match v.Key with
//            | D.TypeLike { Scope = (C.External path as scope) } ->
//                let ctx = C.In scope
//                match v with
//                | TypeDiscovery.DiscoveredTypeEntity x ->
//                    let node = Node(buildEntryTable ctx, x)
//                    Some (path, node)
//                | _ -> None
//            | _ -> None)
        |> Mapping.New
    let rootNode = RootNode(log, pathTable, entryTable)
    NameResolver(rootNode.Resolve) :> INameResolver
