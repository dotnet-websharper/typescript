/// Building upon the results of the Elaboration pass,
/// constructs a table for resolving type-or-module names.
module internal IntelliFactory.TypeScript.NameResolution

open System
open System.Collections.Generic
module E = Elaboration
module L = Logging
module M = Mappings
module Mem = Memoization
module S = Syntax
module T = Tries

type EntityKind =
    | IsClass
    | IsEnum
    | IsInterface
    | IsModule

type Resolution =
    {
        CanonicalLocation : E.Location
        EntityKind : EntityKind
    }

type INameResolver =
    abstract ResolveLocation : E.Location -> option<Resolution>
    abstract ResolveName : E.Context * S.Name -> option<Resolution>

[<AutoOpen>]
module private Implementation =

    let ParentContext (ctx: E.Context) : option<E.Context> =
        match ctx with
        | E.Global -> None
        | E.At (E.Extern _) -> Some E.Global
        | E.At (E.Local (scope, name)) ->
            match name.Parent with
            | None ->
                match scope with
                | E.ExternScope p -> Some (E.At (E.Extern p))
                | E.GlobalScope -> Some E.Global
            | Some p -> Some (E.At (E.Local (scope, p)))

    let rec ContextAncestors (ctx: E.Context) : list<E.Context> =
        match ParentContext ctx with
        | Some p -> ContextAncestorsAndSelf p
        | None -> []

    and ContextAncestorsAndSelf (ctx: E.Context) : list<E.Context> =
        ctx :: ContextAncestors ctx

    type TKey =
        | IdKey of S.Identifier
        | PathKey of S.Path

        override this.ToString() =
            match this with
            | IdKey s -> string s
            | PathKey p -> string p

    type TNode =
        | TImportExternalNode of E.Location * S.Path
        | TImportInternalNode of E.Location * E.Context * S.Name
        | TSimpleNode of E.Location * EntityKind

    type Trie = T.Trie<TKey,TNode>

    let NodeLocation node =
        match node with
        | TImportExternalNode (loc, _)
        | TImportInternalNode (loc, _, _)
        | TSimpleNode (loc, _) -> loc

    let LocationToKeyList (loc: E.Location) : list<TKey> =
        match loc with
        | E.Extern p -> [PathKey p]
        | E.Local (scope, name) ->
            let tail = List.map IdKey (name.List)
            match scope with
            | E.GlobalScope -> tail
            | E.ExternScope p -> PathKey p :: tail

    let RelativeLocation (ctx: E.Context) (name: S.Name) : E.Location =
        match ctx with
        | E.At (E.Extern p) -> E.Local (E.ExternScope p, name)
        | E.At (E.Local (scope, p)) -> E.Local (scope, p.[name])
        | E.Global -> E.Local (E.GlobalScope, name)

    let NameVariants (ctx: E.Context) (name: S.Name) : list<E.Location> =
        ContextAncestorsAndSelf ctx
        |> List.map (fun ctx -> RelativeLocation ctx name)

[<Sealed>]
type private NameResolver(root: Tries.Trie<TKey,TNode>) =

    /// Trie search procedure modified to account for import "symlinks."
    let rec resolve (search: list<TKey>) (t: Trie) : option<Trie> =
        match search with
        | [] -> Some t
        | x :: xs ->
            match T.Lookup x t with
            | None -> None
            | Some tr ->
                match T.Node tr with
                | Some node ->
                    match node with
                    | TImportExternalNode (_, path) ->
                        let m = resolve [PathKey path] root
                        match m with
                        | None -> None
                        | Some m -> resolve xs m
                    | TImportInternalNode (_, ctx, name) ->
                        let m =
                            NameVariants ctx name
                            |> Seq.tryPick (fun loc ->
                                resolve (LocationToKeyList loc) root)
                        match m with
                        | None -> None
                        | Some m -> resolve xs m
                    | TSimpleNode _ ->
                        resolve xs tr
                | None ->
                    resolve xs tr

    let resolveLocation (loc: E.Location) : option<Resolution> =
        match resolve (LocationToKeyList loc) root with
        | None -> None
        | Some tr ->
            match T.Node tr with
            | Some (TSimpleNode (loc, kind)) ->
                Some {
                    CanonicalLocation = loc
                    EntityKind = kind
                }
            | _ -> None

    let resolveLocationTable =
        Mem.Memoize (Mem.Options()) resolveLocation

    member this.ResolveLocation(loc: E.Location) : option<Resolution> =
        resolveLocationTable.[loc]

    member this.ResolveName(ctx: E.Context, name: S.Name) : option<Resolution> =
        NameVariants ctx name
        |> Seq.tryPick this.ResolveLocation

    interface INameResolver with
        member this.ResolveLocation(loc) = this.ResolveLocation(loc)
        member this.ResolveName(ctx, name) = this.ResolveName(ctx, name)

let ConstructResolver (trace: L.Log) (source: S.DeclarationSourceFile) : INameResolver =
    let nodes =
        let simple kind loc = TSimpleNode (loc, kind)
        [|
            for f in Elaboration.Elaborate trace source do
                match f with
                | E.ImportExternal (id, ctx, path) ->
                    yield TImportExternalNode (ctx.[id], path)
                | E.ImportInternal (id, ctx, name) ->
                    yield TImportInternalNode (ctx.[id], ctx, name)
                | E.IsClass loc ->
                    yield simple IsClass loc
                | E.IsEnum loc ->
                    yield simple IsEnum loc
                | E.IsInterface loc ->
                    yield simple IsInterface loc
                | E.IsModule loc ->
                    yield simple IsModule loc
                | E.PathIsDefined p ->
                    yield simple IsModule (E.Extern p)
                | _ -> ()
        |]
    let trieResult =
        nodes
        |> Seq.sortBy (function
            /// Prioritize non-import nodes.
            | TSimpleNode _ -> 0
            | _ -> 1)
        |> Seq.distinct
        |> Seq.map (fun node ->
            (LocationToKeyList (NodeLocation node), node))
        |> T.Construct
    for name in trieResult.Conflicts do
        trace.Warning("Name conflict: {0}", String.concat "/" (Seq.map string name))
    NameResolver(trieResult.Trie) :> INameResolver
