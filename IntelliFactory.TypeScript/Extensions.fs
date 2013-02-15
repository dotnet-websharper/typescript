/// Various utility extension members.
[<AutoOpen>]
module internal IntelliFactory.TypeScript.Extensions

open System
open System.Collections.Generic
open System.IO
open System.Reflection
module M = IntelliFactory.TypeScript.Memoization


/// A wrapper around Lazy<'T> that preserves inner equality.
[<Sealed>]
type Delayed<'T>(x: Lazy<'T>) =
    static let comparer = EqualityComparer<'T>.Default

    member this.Equals(other: Delayed<'T>) =
        comparer.Equals(x.Value, other.Value)

    override this.Equals(other: obj) =
        match other with
        | :? Delayed<'T> as o -> this.Equals(o)
        | _ -> false

    override this.GetHashCode() = comparer.GetHashCode(x.Value)
    override this.ToString() = x.Value.ToString()
    member this.Value = x.Value
    interface IEquatable<Delayed<'T>> with
        member this.Equals(o) = this.Equals(o)

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

module Mapping =

    let GetPairs (input: Mapping<_,_>) =
        input.GetPairs()

    let Reverse (input: Mapping<'T1,'T2>) : Mapping<'T2,'T1> =
        let pairs =
            input.GetPairs()
            |> Seq.map (fun (k, v) -> (v, k))
        Mapping(pairs)

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

module Seq =

    let MergeDuplicates (project: 'T -> 'U)
                        (comparer: IEqualityComparer<'U>)
                        (merge: 'T -> 'T -> 'T)
                        (sequence: seq<'T>) : seq<'T> =
        let d = Dictionary<'U,'T>(comparer)
        let sequence = Seq.cache sequence
        let total = Seq.length sequence
        for x in sequence do
            let key = project x
            match d.TryGetValue(key) with
            | true, y -> d.[key] <- merge x y
            | _ -> d.Add(key, x)
        d.Values :> seq<'T>

module List =

    let rec Inits (el: list<'T>) : list<list<'T>> =
        match el with
        | [] -> [[]]
        | x :: xs -> [] :: [for rest in Inits xs -> x :: rest]

    let rec Explode (interp: 'T -> list<'R>) (input: list<'T>) : list<list<'R>> =
        match input with
        | [] -> [[]]
        | x :: xs ->
            [
                for h in interp x do
                    for t in Explode interp xs do
                        yield h :: t
            ]

module Assembly =

    /// See http://stackoverflow.com/questions/10357273/type-provider-calling-another-dll-in-f
    let InstallLoadHack () =
        AppDomain.CurrentDomain.add_AssemblyResolve(fun _ args ->
            let name = AssemblyName(args.Name)
            let existingAssembly =
                AppDomain.CurrentDomain.GetAssemblies()
                |> Seq.tryFind(fun a ->
                    AssemblyName.ReferenceMatchesDefinition(name, a.GetName()))
            defaultArg existingAssembly null)
