/// Implements hash-consed checked symbols with fast O(1) equality, and string-based comparison.
module IntelliFactory.TypeScript.Symbols

open System
open System.Text.RegularExpressions
open Microsoft.CSharp
module M = IntelliFactory.TypeScript.Memoization

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
type Symbol<'T when 'T :> ISymbolChecker and 'T : (new : unit -> 'T)> private (name: string) =
    static let reference = HashIdentity.Reference
    static let checker = new 'T()
    static let make (name: string) = Symbol<'T>(checker.Check(name))
    static let table = M.Memoize (M.Options()) make
    member this.CompareTo(other: Symbol<'T>) = compare name other.Name
    override this.Equals(other) = Object.ReferenceEquals(this, other)
    override this.GetHashCode() = reference.GetHashCode(this)
    override this.ToString() = name
    member this.Name = name
    static member Create(text: string) : Symbol<'T> = table.[text]
    interface IComparable with
        member this.CompareTo(other) = this.CompareTo(other :?> _)
    interface IComparable<Symbol<'T>> with
        member this.CompareTo(other) = this.CompareTo(other)
    interface IEquatable<Symbol<'T>> with
        member this.Equals(other) = Object.ReferenceEquals(this, other)

/// Represents qualified names, semantically equivalent to non-empty lists of symbols.
/// Names are also hash-consed and have fast equality.
[<Sealed>]
type Name<'T when 'T :> ISymbolChecker and 'T : (new : unit -> 'T)> private (node: NameNode<'T>) =

    static let checker = new 'T()
    static let reference = HashIdentity.Reference
    static let table = M.Memoize (M.Options()) (fun x -> Name<'T>(x))

    static let rec toList acc node =
        match node with
        | GlobalNameNode name -> name :: acc
        | NestedNameNode (name, local) -> toList (local :: acc) name.Node

    let list = lazy toList [] node
    let text = lazy checker.Join(list.Value |> Seq.map (fun x -> x.Name))

    member this.CompareTo(other: Name<'T>) = compare this.List other.List
    override this.Equals(other) = Object.ReferenceEquals(this, other)
    override this.GetHashCode() = reference.GetHashCode(this)

    member this.Match() =
        match node with
        | GlobalNameNode s -> Choice1Of2 s
        | NestedNameNode (a, b) -> Choice2Of2 (a, b)

    override this.ToString() = text.Value

    member this.Item
        with get (x: string) : Name<'T> =
            Name<'T>.Nested(this, Symbol<'T>.Create(x))

    member this.Item
        with get (x: Symbol<'T>) : Name<'T>=
            Name<'T>.Nested(this, x)

    member this.Item
        with get (name: Name<'T>) : Name<'T> =
            (this, name.List)
            ||> List.fold (fun a b -> a.[b])

    member this.List : List<Symbol<'T>> = list.Value

    member this.Local =
        match node with
        | GlobalNameNode s
        | NestedNameNode (_, s) -> s

    member private this.Node : NameNode<'T> = node

    member this.Parent =
        match node with
        | GlobalNameNode _ -> None
        | NestedNameNode (n, _) -> Some n

    member this.Text = text.Value
    static member Global(sym) = table.[GlobalNameNode sym]
    static member Nested(sym, name) = table.[NestedNameNode (sym, name)]

    interface IComparable with
        member this.CompareTo(other) = this.CompareTo(other :?> _)
    interface IComparable<Name<'T>> with
        member this.CompareTo(other) = this.CompareTo(other)
    interface IEquatable<Name<'T>> with
        member this.Equals(other) = Object.ReferenceEquals(this, other)

and private NameNode<'T when 'T :> ISymbolChecker and 'T : (new : unit -> 'T)> =
    | GlobalNameNode of Symbol<'T>
    | NestedNameNode of Name<'T> * Symbol<'T>

let ConvertSymbol (a: Symbol<'A>) : Symbol<'B> =
    Symbol<'B>.Create(a.Name)

let ConvertName (a: Name<'A>) : Name<'B> =
    (Name<'B>.Global (ConvertSymbol a.List.Head), a.List.Tail)
    ||> List.fold (fun x y -> x.[ConvertSymbol y])

let (|GlobalName|LocalName|) (name: Name<'A>) =
    name.Match()

[<Sealed>]
type SyntaxChecker() =
    interface ISymbolChecker with
        member this.Join(ss) = String.concat "." ss
        member this.Check(name) =
            if String.IsNullOrWhiteSpace(name) then
                nullArg "name"
            name

[<Sealed>]
type NetChecker() =

    static let provider = CSharpCodeProvider.CreateProvider("C#")
    static let badChar = Regex(@"[^\w]")

    static let clean (name: string) : string =
        match name with
        | null | "" -> invalidArg "name" "Invalid .NET name: empty name"
        | _ ->
            let s0 =
                if Char.IsLetter(name.[0]) || name.[0] = '_'
                    then string name.[0]
                    else "_"
            let sR = badChar.Replace(name.Substring(1), "_")
            let n = s0.ToUpper() + sR
            let rec loop x =
                if provider.IsValidIdentifier(x)
                    then x
                    else loop ("_" + x)
            loop n

    interface ISymbolChecker with
        override this.Check(name) = clean name
        override this.Join(ss) = String.concat "." ss
