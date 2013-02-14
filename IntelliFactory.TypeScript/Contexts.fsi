/// Defines contexts for name resolution.
module internal IntelliFactory.TypeScript.Contexts

module S = Syntax

/// External module paths.
type Path = string

[<Sealed>]
type Location =
    member ToContext : unit -> Context
    static member External : Path -> Location

and [<Sealed>] Context =
    member Ancestors : unit -> list<Context>
    member AncestorsAndSelf : unit -> list<Context>
    member RelativeContext : S.Identifier -> Context
    member RelativeContext : S.Name -> Context
    member RelativeLocation : S.Identifier -> Location
    member RelativeLocation : S.Name -> Location
    member Parent : option<Context>

    static member External : Path -> Context
    static member Global : Context

val (|External|Global|Nested|) : Context -> Choice<Path,unit,Context * S.Identifier>
val (|At|Extern|) : Location -> Choice<Context * S.Identifier,Path>
