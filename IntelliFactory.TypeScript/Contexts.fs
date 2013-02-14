/// Implements contexts for syntactic name resolution in TypeScript.
module IntelliFactory.TypeScript.Contexts

open System
module M = Memoization
module S = Syntax

type Path = string

[<Sealed>]
type Context private (node: ContextNode) =
    static let make = M.Memoize (M.Options()) (fun node -> Context(node))
    static let globalContext = make.[Global]
    let text = string node
    let parent =
        match node with
        | External _ -> Some globalContext
        | Global -> None
        | Nested (x, _) -> Some x

    member this.Ancestors() : list<Context> =
        match this.Parent with
        | Some p -> p.AncestorsAndSelf()
        | None -> []

    member this.AncestorsAndSelf() : list<Context> =
        this :: this.Ancestors()

    member this.RelativeContext(x: S.Identifier) : Context =
        Context.Create (Nested (this, x))

    member this.RelativeContext(n: S.Name) : Context =
        match n with
        | S.GlobalName x -> Context.Create (Nested (this, x))
        | S.LocalName (n, x) -> Context.Create (Nested (this.RelativeContext(n), x))

    member this.RelativeLocation(n: S.Identifier) : Location =
        Location.Create (At (this, n))

    member this.RelativeLocation(n: S.Name) : Location =
        match n with
        | S.GlobalName x -> Location.Create (At (this, x))
        | S.LocalName (n, x) -> Location.Create (At (this.RelativeContext(n), x))

    override this.ToString() = text

    member this.Node = node
    member this.Parent : option<Context> = parent
    static member Create(node: ContextNode) : Context = make.[node]
    static member External(path: Path) = Context.Create (External path)
    static member Global = globalContext

and ContextNode =
    | External of Path
    | Global
    | Nested of Context * S.Identifier

    override this.ToString() =
        match this with
        | External path -> String.Format("ext:{0}", path)
        | Global -> String.Format("^")
        | Nested (c, id) -> String.Format("{0}/{1}", c, id)

and [<Sealed>] Location private (node: LocationNode) =
    static let make = M.Memoize (M.Options()) (fun node -> Location(node))
    let text = string node

    member this.ToContext() =
        match node with
        | At (context, id) -> context.RelativeContext(id)
        | Extern path -> Context.External(path)

    override this.ToString() = text
    member this.Node = node
    static member Create(node: LocationNode) : Location = make.[node]
    static member External(path: Path) = Location.Create (Extern path)

and LocationNode =
    | At of Context * S.Identifier
    | Extern of Path

    override this.ToString() =
        match this with
        | At (ctx, id) -> String.Format("{0}/{1}", ctx, id)
        | Extern path -> String.Format("in-ext:{0}", path)

let (|External|Global|Nested|) (c: Context) =
    match c.Node with
    | External p -> External p
    | Global -> Global
    | Nested (a, b) -> Nested (a, b)

let (|At|Extern|) (loc: Location) =
    match loc.Node with
    | At (a, b) -> At (a, b)
    | Extern p -> Extern p
