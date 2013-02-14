/// Takes care of resolving comment-based imports in TypeScript files.
module internal IntelliFactory.TypeScript.Resolver

open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Text.RegularExpressions
open IntelliFactory.Parsec
open IntelliFactory.TypeScript

let private networkPathPattern =
    Regex(@"^(\w+\:\/\/)")

let private absolutePathPattern =
    Regex(@"^(\w+\:|[\\/])")

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
        if absolutePathPattern.IsMatch(relative) then
            Location.Create(relative)
        else
            match this with
            | FileLocation (dir, _) ->
                Location.Create(Path.Combine(dir, relative))
            | NetworkLocation x ->
                Location.Create(Uri(Uri(x), relative))

    static member private Create(u: Uri) =
        NetworkLocation (string u)

    /// Creates a new location.
    static member Create(spec: string) =
        if networkPathPattern.IsMatch(spec) then
            Location.Create(Uri(spec, UriKind.Absolute))
        else
            let abs = Path.GetFullPath(spec)
            let dir = Path.GetDirectoryName(abs)
            let file = Path.GetFileName(abs)
            FileLocation(dir, file)

let private load (log: Logging.Log) (loc: Location) =
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
        log.Warning("Failed to load: {0}", loc)
        None

let Resolve (log: Logging.Log) (loc: Location) : Lexer.Lexeme [] =
    let visited = HashSet()
    let rec loop (acc: list<Lexer.Lexeme[]>) (loc: Location) : list<Lexer.Lexeme[]> =
        if visited.Add(loc) then
            match load log loc with
            | Some text ->
                let input = ParseInput.FromString [] (string loc) text
                match Lexer.Lex.Parse input with
                | ParseFailed f ->
                    log.Warning("Failed to process {0}: {1}", loc, f)
                    acc
                | Parsed (r, _, refs, _) ->
                    seq { for x in refs -> loc.Resolve(x) }
                    |> Seq.fold loop (r :: acc)
            | None -> acc
        else acc
    Array.concat (loop [] loc)
