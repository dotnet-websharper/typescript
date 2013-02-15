/// Implements contexts for syntactic name resolution in TypeScript.
module IntelliFactory.TypeScript.Contexts

open System
module M = Memoization
module S = Syntax

type Path = string

type Scope =
    | External of Path
    | Global

    override this.ToString() =
        match this with
        | External p -> String.Format("[{0}]", p)
        | Global -> "^"

type Location =
    {
        Name : S.Name
        Scope : Scope
    }

    override this.ToString() =
        String.Format("{0}:{1}", this.Scope, this.Name)

    member this.Item
        with get (name: S.Name) : Location =
            { this with Name = this.Name.[name] }

    member this.Item
        with get (id: S.Identifier) : Location =
            { this with Name = this.Name.[id] }


type Context =
    | At of Location
    | In of Scope

    member this.Ancestors() : list<Context> =
        match this.Parent with
        | Some p -> p.AncestorsAndSelf()
        | None -> []

    member this.AncestorsAndSelf() : list<Context> =
        this :: this.Ancestors()

    member this.Parent : option<Context> =
        match this with
        | In _ -> None
        | At { Scope = scope; Name = name } ->
            match name.Parent with
            | None -> In scope
            | Some p -> At { Scope = scope; Name = p }
            |> Some

    member this.Item
        with get (id: S.Identifier) : Location =
            match this with
            | At loc -> loc.[id]
            | In scope -> { Scope = scope; Name = Syntax.Name.Global id }

    member this.Item
        with get (name: S.Name) : Location =
            match this with
            | At loc -> loc.[name]
            | In scope -> { Scope = scope; Name = name }
