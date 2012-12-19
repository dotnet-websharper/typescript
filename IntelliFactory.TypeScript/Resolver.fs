module internal IntelliFactory.TypeScript.Declarations.Resolver

open System
open System.Collections.Generic
open System.IO
open System.Net
open IntelliFactory.Parsec
open IntelliFactory.TypeScript

/// Represents network and local filesystem locations.
type Location =
    private
    | FileLocation of string * string
    | NetworkLocation of string

    override this.ToString() =
        match this with
        | FileLocation (a, b) -> Path.Combine(a, b)
        | NetworkLocation x -> x

    /// Resolves a relative location.
    member this.Resolve(relative: string) =
        match this with
        | FileLocation (dir, _) ->
            Location.Create(Path.Combine(dir, relative))
        | NetworkLocation x ->
            Location.Create(Uri(Uri(x), relative))

    static member private Create(u: Uri) =
        NetworkLocation (string u)

    /// Creates a new location.
    static member Create(spec: string) =
        if File.Exists(spec) then
            let abs = Path.GetFullPath(spec)
            let dir = Path.GetDirectoryName(abs)
            let file = Path.GetFileName(abs)
            FileLocation(dir, file)
        else
            let u = Uri(spec, UriKind.Absolute)
            Location.Create(u)

let private load (log: Log) (loc: Location) =
    try
        match loc with
        | FileLocation (d, f) ->
            let p = Path.Combine(d, f)
            File.ReadAllText(p)
        | NetworkLocation x ->
            use client = new WebClient()
            client.DownloadString(x)
        |> Some
    with _ ->
        log.Warn("Failed to load: {0}", loc)
        None

let Resolve (log: Log) (loc: Location) : Lexer.Lexeme [] =
    let visited = HashSet()
    let rec loop (acc: list<Lexer.Lexeme[]>) (loc: Location) : list<Lexer.Lexeme[]> =
        if visited.Add(loc) then
            match load log loc with
            | Some text ->
                let input = ParseInput.FromString [] (string loc) text
                match Lexer.Lex.Parse input with
                | ParseFailed f ->
                    log.Warn("Failed to process {0}: {1}", loc, f)
                    acc
                | Parsed (r, _, refs, _) ->
                    seq { for x in refs -> loc.Resolve(x) }
                    |> Seq.fold loop (r :: acc)
            | None -> acc
        else acc
    Array.concat (loop [] loc)
