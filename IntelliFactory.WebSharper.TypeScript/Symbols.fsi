/// Implements hash-consed checked symbols with fast O(1) equality, and string-based comparison.
module internal IntelliFactory.TypeScript.Symbols

open System

/// Implements checking and rewriting of symbol names.
type ISymbolChecker =

    /// Checks a suggested name and either throws an exception or normalizes it
    /// if the name is not a valid identifier according to the language rules.
    abstract Check : string -> string

    /// Joins textual representations of symbols to obtain a textual
    /// representation for a qualified name.
    abstract Join : seq<string> -> string

/// Custom symbol type parameterized by symbol checking rules.
[<Sealed>]
type Symbol<'T when 'T :> ISymbolChecker and 'T : (new : unit -> 'T)> =
    member Name : string
    static member Create : text: string -> Symbol<'T>
    interface IComparable
    interface IComparable<Symbol<'T>>
    interface IEquatable<Symbol<'T>>

/// Represents qualified names, semantically equivalent to non-empty lists of symbols.
/// Names are also hash-consed and have fast equality.
[<Sealed>]
type Name<'T when 'T :> ISymbolChecker and 'T : (new : unit -> 'T)> =
    member List : List<Symbol<'T>>
    member Local : Symbol<'T>
    member Parent : option<Name<'T>>
    member Text : string
    static member Global : Symbol<'T> -> Name<'T>
    static member Nested : Name<'T> * Symbol<'T> -> Name<'T>

    member Item : string -> Name<'T> with get
    member Item : Symbol<'T> -> Name<'T> with get
    member Item : Name<'T> -> Name<'T> with get

    interface IComparable
    interface IComparable<Name<'T>>
    interface IEquatable<Name<'T>>

val (|GlobalName|LocalName|) : Name<'T> -> Choice<Symbol<'T>, Name<'T> * Symbol<'T>>
val ConvertName : Name<'A> -> Name<'B>
val ConvertSymbol : Symbol<'A> -> Symbol<'B>

[<Sealed>]
type SyntaxChecker =
    new : unit -> SyntaxChecker
    interface ISymbolChecker

[<Sealed>]
type NetChecker =
    new : unit -> NetChecker
    interface ISymbolChecker

module Syntax =
    type Identifier = Symbol<SyntaxChecker>
    type Name = Name<SyntaxChecker>

module Net =
    type Identifier = Symbol<NetChecker>
    type Name = Name<NetChecker>
