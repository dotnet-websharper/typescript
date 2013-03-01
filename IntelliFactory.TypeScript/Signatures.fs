/// Deals with custom equality and hashing of vararg signatures.
module internal IntelliFactory.TypeScript.Signatures

/// Signatures are defined as a finite number of types and an optional rest type.
/// The semantics of a signature is the set of all possible type lists obtained
/// by expanding the rest type, if available, an arbitrary number of times.
type Signature<'T> =
    | Empty
    | Rest of 'T
    | With of 'T * Signature<'T>

/// Changes the type of the signature.
let rec Map (f: 'A -> 'B) (s: Signature<'A>) : Signature<'B> =
    match s with
    | Empty -> Empty
    | Rest x -> Rest (f x)
    | With (x, xs) -> With (f x, Map f xs)

/// Splits the signature into regular parameters and the rest parameter.
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



