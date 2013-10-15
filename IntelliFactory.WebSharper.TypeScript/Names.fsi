namespace IntelliFactory.WebSharper.TypeScript

/// Implements identifiers and qualified names.
module Names =
    open System

    /// Represents a name with fast equality and comparison.
    [<Sealed>]
    type Name =
        interface IComparable

        /// The textual form of the name.
        member Text : string

    /// Builds names with hash-consing.
    [<Sealed>]
    type NameBuilder =

        /// Creates a new name.
        member CreateName : string -> Name

        /// Hash-conses a string.
        member ShareString : string -> string

        /// Creates a new name builder.
        static member Create : unit -> NameBuilder
