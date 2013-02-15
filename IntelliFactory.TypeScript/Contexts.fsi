/// Implements contexts for syntactic name resolution in TypeScript.
module internal IntelliFactory.TypeScript.Contexts

type Path = string
module S = Syntax

type Scope =
    | External of Path
    | Global

type Location =
    {
        Name : S.Name
        Scope : Scope
    }

    member Item : S.Identifier -> Location with get
    member Item : S.Name -> Location with get

type Context =
    | At of Location
    | In of Scope

    member Ancestors : unit -> list<Context>
    member AncestorsAndSelf : unit -> list<Context>
    member Parent : option<Context>

    member Item : S.Identifier -> Location with get
    member Item : S.Name -> Location with get
