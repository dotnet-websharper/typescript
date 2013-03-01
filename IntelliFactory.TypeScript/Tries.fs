module internal IntelliFactory.TypeScript.Tries

open System.Collections.Generic

[<NoComparison>]
[<ReferenceEquality>]
type Trie<'K,'V> =
    private {
        mutable Node : option<'V>
        Table : IDictionary<'K,Trie<'K,'V>>
    }

let private Fresh () =
    { Node = None; Table = Dictionary() }

let Lookup (key: 'K) (t: Trie<'K,'V>) : option<Trie<'K,'V>> =
    match t.Table.TryGetValue(key) with
    | true, tr -> Some tr
    | _ -> None

let rec LookupTrie (key: list<'K>) (t: Trie<'K,'V>) : option<Trie<'K,'V>> =
    match key with
    | [] -> Some t
    | k :: ks ->
        match Lookup k t with
        | None -> None
        | Some tr -> LookupTrie ks tr

let LookupNode (key: list<'K>) (t: Trie<'K,'V>) : option<'V> =
    match LookupTrie key t with
    | None -> None
    | Some t -> t.Node

let Node (t: Trie<'K,'V>) : option<'V> =
    t.Node

let rec private Add (key: list<'K>) (value: 'V) (t: Trie<'K,'V>) : bool =
    match key with
    | [] ->
        match t.Node with
        | None -> t.Node <- Some value; true
        | Some _ -> false
    | k :: ks ->
        let tr =
            match t.Table.TryGetValue(k) with
            | true, tr -> tr
            | _ ->
                let tr = Fresh ()
                t.Table.Add(k, tr)
                tr
        Add ks value tr

type ConstructedTrie<'K,'V> =
    {
        Conflicts : seq<list<'K>>
        Trie : Trie<'K,'V>
    }

let Construct (entries: seq<list<'K> * 'V>) : ConstructedTrie<'K,'V> =
    let tr = Fresh()
    let conflicts =
        seq {
            for (k, v) in entries do
                if not (Add k v tr) then
                    yield k
        }
        |> Seq.distinct
        |> Seq.toArray
    {
        Conflicts = conflicts :> seq<_>
        Trie = tr
    }

