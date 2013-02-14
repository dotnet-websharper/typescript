/// Implements memoization tables.
module internal IntelliFactory.TypeScript.Memoization

open System
open System.Collections.Generic

[<Sealed>]
type Options<'T1,'T2>() =
    let mutable equality : IEqualityComparer<'T1> = EqualityComparer<'T1>.Default :> _
    let mutable laziness : option<Lazy<'T2> -> 'T2> = None

    member this.Equality
        with get () = equality
        and set x = equality <- x

    member this.Laziness
        with get () = laziness
        and set x = laziness <- x

[<NoComparison>]
[<NoEquality>]
type Table<'T1,'T2> =
    private {
        Dictionary : Dictionary<'T1,'T2>
        Lookup : 'T1 -> 'T2
        Options : Options<'T1,'T2>
        Root : obj
    }

    member this.Clear() =
        lock this.Root <| fun () ->
            this.Dictionary.Clear()

    member this.Get(key: 'T1) : 'T2 =
        this.Lookup key

    member this.GetKeys() : seq<'T1> =
        fun () ->
            let keys = this.Dictionary.Keys
            let a = Array.zeroCreate keys.Count
            keys.CopyTo(a, 0)
            a
        |> lock this.Root
        |> Array.toSeq

    member this.Remove(key: 'T1) : bool =
        lock this.Root <| fun () ->
            this.Dictionary.Remove(key)

    member this.Comparer : IEqualityComparer<'T1> =
        this.Options.Equality

    member this.Count =
        lock this.Root <| fun () ->
            this.Dictionary.Count

    member this.Item
        with get (key: 'T1) : 'T2 =
            this.Lookup key

let Memoize<'T1,'T2> (cfg: Options<'T1,'T2>) (f: 'T1 -> 'T2) : Table<'T1,'T2> =
    let cache = Dictionary(cfg.Equality)
    let root = obj ()
    let lookup =
        match cfg.Laziness with
        | Some pack ->
            fun x ->
                let r =
                    lock root <| fun () ->
                        match cache.TryGetValue(x) with
                        | true, y -> Lazy.CreateFromValue(y)
                        | _ ->
                            let r = lazy f x
                            cache.[x] <- pack r
                            r
                r.Value
        | None ->
            fun x ->
                lock root <| fun () ->
                    match cache.TryGetValue(x) with
                    | true, y -> y
                    | _ ->
                        let r = f x
                        cache.[x] <- r
                        r
    {
        Dictionary = cache
        Lookup = lookup
        Options = cfg
        Root = root
    }

let MemoizeRecursive<'T1,'T2> (cfg: Options<'T1,'T2>) (f: ('T1 -> 'T2) -> ('T1 -> 'T2)) : Table<'T1,'T2> =
    let rec g x = f h.Get x
    and h : Table<'T1,'T2> = Memoize cfg g
    h
