module internal IntelliFactory.TypeScript.Mappings

open System.Collections.Generic

[<Sealed>]
type Mapping<'T1,'T2> private (d: IDictionary<'T1,'T2>) =
    static let cmp1 = EqualityComparer<'T1>.Default
    static let cmp2 = EqualityComparer<'T2>.Default

    new () =
        Mapping(Dictionary(cmp1))

    new (s: seq<'T1 * 'T2>) =
        let d = Dictionary(cmp1)
        s |> Seq.iter (fun (k, v) -> d.[k] <- v)
        Mapping(d)

    member private this.Data = d

    member this.GetKeys() =
        let a = Array.zeroCreate d.Count
        d.Keys.CopyTo(a, 0)
        a :> seq<_>

    member this.GetPairs() =
        d
        |> Seq.map (fun (KeyValue (k, v)) -> (k, v))

    member this.GetValues() =
        let a = Array.zeroCreate d.Count
        d.Values.CopyTo(a, 0)
        a :> seq<_>

    member this.TryFind(x: 'T1) : option<'T2> =
        match d.TryGetValue(x) with
        | true, x -> Some x
        | _ -> None

    member this.Count = d.Count
    member this.Item with get x = d.[x]

    override this.GetHashCode() =
        let ( +++ ) a b = (a <<< 5) + a + b
        Seq.fold (fun x (KeyValue (k, v)) -> x +++ cmp1.GetHashCode(k) +++ cmp2.GetHashCode(v)) 5381 d

    override this.Equals(other: obj) =
        match other with
        | :? Mapping<'T1,'T2> as m ->
            let o = m.Data
            d.Count = o.Count
            &&  d
                |> Seq.forall (fun kv ->
                    match o.TryGetValue(kv.Key) with
                    | true, x -> cmp2.Equals(x, kv.Value)
                    | _ -> false)
        | _ -> false

let TryFind key (input: Mapping<_,_>) =
    input.TryFind(key)

let GetCount (input: Mapping<_,_>) =
    input.Count

let GetKeys (input: Mapping<_,_>) =
    input.GetKeys()

let GetPairs (input: Mapping<_,_>) =
    input.GetPairs()

let GetValues (input: Mapping<_,_>) =
    input.GetKeys()

let New pairs =
    Mapping(pairs)

let Map (f: 'K -> 'T1 -> 'T2) (a: Mapping<'K,'T1>) : Mapping<'K,'T2> =
    a.GetPairs()
    |> Seq.map (fun (a, b) -> (a, f a b))
    |> New

let Choose (f: 'K -> 'T1 -> option<'T2>) (a: Mapping<'K,'T1>) : Mapping<'K,'T2> =
    a.GetPairs()
    |> Seq.choose (fun (a, b) ->
        match f a b with
        | Some c -> Some (a, c)
        | None -> None)
    |> New

let Singleton (k: 'T1) (v: 'T2) : Mapping<'T1,'T2> =
    New [k, v]

let Empty<'T1,'T2> : Mapping<'T1,'T2> =
    New []

let Union (ms: seq<Mapping<'A,'B>>) : Mapping<'A,'B> =
    New (Seq.collect GetPairs ms)

let UnionWith (u: seq<'B> -> 'B) (ms: seq<Mapping<'A,'B>>) : Mapping<'A,'B> =
    Seq.collect GetPairs ms
    |> Seq.groupBy fst
    |> Seq.map (fun (k, v) -> (k, u (Seq.map snd v)))
    |> New
