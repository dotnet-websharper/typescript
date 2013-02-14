/// Deals with custom equality and hashing of vararg signatures.
module internal IntelliFactory.TypeScript.Signatures

open System
open System.Collections.Generic

/// DJB hash function.
let inline private ( +++ ) a b =
    (a <<< 5) + a + b

/// Signatures are defined as a finite number of types and an optional rest type.
/// The semantics of a signature is the set of all possible type lists obtained
/// by expanding the rest type, if available, an arbitrary number of times.
type Signature<'T> =
    | Empty
    | Rest of 'T
    | With of 'T * Signature<'T>

let Split (s: Signature<'T>) : list<'T> * option<'T> =
    let rec sp acc x =
        match x with
        | Empty -> (List.rev acc, None)
        | Rest t -> (List.rev acc, Some t)
        | With (x, xs) -> sp (x :: acc) xs
    sp [] s

/// Given a decidable subtyping relation on component types, subtyping on signatures
/// is defined A <: B holds when every possible expansion of A is
/// pointwise subsumed by some expansion in B.
[<Sealed>]
type Subtyping<'T>(isA: 'T -> 'T -> bool) =
    let rec fits a b =
        match a, b with
        | Empty, Empty | Empty, Rest _ -> true
        | Rest _, Empty | Empty, With _ | With _, Empty | Rest _, With _ -> false
        | Rest x, Rest y -> isA x y
        | With (x, xs), (Rest y as r) -> isA x y && fits xs r
        | With (x, xs), With (y, ys) -> isA x y && fits xs ys

    /// Decides the subtyping relation on signatures.
    member this.IsA(a, b) = fits a b

//    override this.GetHashCode() =
//        Equality<'T>.Default.GetHashCode(this)
//
//    override this.Equals(other: obj) =
//        match other with
//        | :? Signature<'T> as o -> Equality<'T>.Default.Equals(this, o)
//        | _ -> false

/// Equality on signatures - given a subtyping,  S :> T
//[<Sealed>]
//type Equality<'T>(c: IEqualityComparer<'T>) =
//    class
//    end

//
//    static let def : Equality<'T> = Equality<'T>(EqualityComparer<'T>.Default)
//
//    let rec ( =. ) a b =
//        match a, b with
//        | Empty, Empty | Rest _, Empty | Empty, Rest _ -> true
//        | Empty, With _ | With _, Empty -> false
//        | Rest x, Rest y -> c.Equals(x, y)
//        | (Rest x as r), With (y, ys) | With (y, ys), (Rest x as r) -> c.Equals(x, y) && r =. ys
//        | With (x, xs), With (y, ys) -> c.Equals(x, y) && xs =. ys
//
//    let rec ( /. ) (s: Signature<'T>) (limit: int)  =
//        match limit with
//        | 0 -> 1
//        | _ ->
//            match s with
//            | Empty  -> 2
//            | Rest t -> 3 +++ c.GetHashCode(t) +++ s /. (limit - 1)
//            | With (t, s) -> 3 +++ c.GetHashCode(t) +++ s /. (limit - 1)
//
//    member this.Equals(a, b) = a =. b
//    member this.GetHashCode(s: Signature<'T>) = 5381 +++ s /. 5
//
//    interface IEqualityComparer<Signature<'T>> with
//        member this.Equals(a, b) = this.Equals(a, b)
//        member this.GetHashCode(s) = this.GetHashCode(s)
//
//    static member Default : Equality<'T> = def
//
//type T =
//    | Nat
//    | Int
//    | Num
//    | Str
//
//let isA a b =
//    match a, b with
//    | Int, Num -> true
//    | Nat, Num -> true
//    | Nat, Int -> true
//    | _ -> false
//
//let st = Subtyping<T>(isA)
//
//let s1 = Empty
//let s2 = Empty
//st.IsA(With (Int, With (Int, Rest Nat)), Rest Num)



//    member this.GetLength() =
//        let rec loop s =
//            match s with
//            | SignatureEmpty -> 1
//            | SignatureRepeat _ -> 0
//            | SignatureWith (_, s) -> 1 + loop s
//        loop this
//
//    member this.GetParameters() =
//        let rec loop s =
//            match s with
//            | SignatureWith (t, s) -> t :: loop s
//            | _ -> []
//        loop this
//
//    member this.GetRest() =
//        let rec loop s =
//            match s with
//            | SignatureEmpty -> None
//            | SignatureRepeat x -> Some x
//            | SignatureWith (_, s) -> loop s
//        loop this
//
//
//
//    override this.GetHashCode() =
//        Signature.Hashing(this)
//
//    override this.Equals(other: obj) =
//        match other with
//        | :? Signature as s -> Signature.Equality(this, s)
//        | _ -> false
