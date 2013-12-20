// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

namespace IntelliFactory.WebSharper.TypeScript

module E = ExternalModuleNames
module P = Parser
module S = Syntax

/// Implements source file dependency resolution
/// according to section 11.1.1 of TypeScript manual 0.9.5.
module SourceFileDependencies =

    type Resolver =
        {
            Compute : FilePath -> option<E.TopLevelName>
            Find : E.TopLevelName -> option<FilePath>
        }

        member r.ComputeName(n) = r.Compute n
        member r.Resolve(n) = r.Find n


    type Path = System.IO.Path
    type FilePath = string

    let getRelativePath (dir: FilePath) (file: FilePath) =
        let dir = Path.GetFullPath(dir)
        let file = Path.GetFullPath(file)
        if file.StartsWith(dir) && file.Length > dir.Length then
            file.Substring(dir.Length).TrimStart(Path.DirectorySeparatorChar) |> Some
        else None

    let trimSuffix (s: string) =
        if s.EndsWith(".d.ts") then
            s.Substring(0, s.Length - ".d.ts.".Length) |> Some
        else None

    let getTopLevelNameFromRelativePath dir file =
        let ( >>= ) a b = Option.bind b a
        getRelativePath dir file
        >>= trimSuffix
        >>= E.Name.TryParse
        >>= function
            | E.TopLevel x -> Some x
            | _ -> None

    type Resolver with

        static member Create(find, compute) =
            { Compute = compute; Find = find }

        static member Combine(resolvers: seq<Resolver>) =
            let resolvers = Seq.toArray resolvers
            let find name =
                resolvers
                |> Array.tryPick (fun r -> r.Resolve(name))
            let compute path =
                resolvers
                |> Array.tryPick (fun r -> r.ComputeName(path))
            Resolver.Create(find, compute)

        static member InFolder(dir) =
            let find (name: E.TopLevelName) =
                let path = Path.Combine(dir, name.Text + ".d.ts")
                if FileInfo(path).Exists
                    then Some path
                    else None
            Resolver.Create(find, getTopLevelNameFromRelativePath dir)

        static member InFolders(paths) =
            seq { for p in paths -> Resolver.InFolder p }
            |> Resolver.Combine

        static member Failure =
            {
                Compute = fun _ -> None
                Find = fun _ -> None
            }

    [<NoComparison>]
    [<NoEquality>]
    type Config =
        {
            Resolver : Resolver
            StartFiles : seq<FilePath>
        }

    type Warning =
        | Missing of FilePath
        | NoParse of FilePath * string
        | UnknownFileType of FilePath
        | Unresolved of E.TopLevelName

        override this.ToString() =
            match this with
            | Missing fP -> sprintf "Missing: %s" fP
            | NoParse (fP, reason) -> sprintf "NoParse [%s]: %s" fP reason
            | UnknownFileType fP -> sprintf "UnknownFileType: %s" fP
            | Unresolved name -> sprintf "Unresovled: %O" name

    [<Sealed>]
    type SourceFile(path: FilePath, syntax: S.DeclarationSourceFile, ?name: E.TopLevelName) =
        member sf.FilePath = path
        member sf.ModuleName = name
        member sf.Syntax = syntax

    [<Sealed>]
    type Result(sf: SourceFile[], ws: Warning[]) =
        member r.HasWarnings = ws.Length > 0
        member r.SourceFiles = sf :> seq<_>
        member r.Warnings = ws :> seq<_>

    let Configure paths =
        {
            Resolver = Resolver.InFolder(".")
            StartFiles = Seq.toArray paths :> seq<_>
        }

    type State =
        {
            NameBuilder : Names.NameBuilder
            Parsed : Map<FilePath,SourceFile>
            ToConsider : Map<FilePath,option<E.TopLevelName>>
            ToResolve : Set<E.TopLevelName>
            Warnings : list<Warning>
        }

    let init () =
        {
            NameBuilder = Names.NameBuilder.Create()
            Parsed = Map.empty
            ToConsider = Map.empty
            ToResolve = Set.empty
            Warnings = []
        }

    let warn warning state =
        { state with Warnings = warning :: state.Warnings }

    let findExternImports (S.DSF dsf) =
        seq {
            for decl in dsf do
                match decl with
                | S.DE4 (_, S.EID (_, name)) ->
                    yield name
                | S.DE5  (_, S.AD6 (S.AEMD (_, body))) ->
                    for e in body do
                        match e with
                        | S.AEME3 (_, S.EID (_, name)) ->
                            yield E.TopLevel name
                        | _ -> ()
                | _ -> ()
        }
        |> Seq.distinct
        |> Seq.cache

    let withFileToConsider path name state =
        let path = Path.GetFullPath(path)
        if state.Parsed.ContainsKey(path) then state else
            let tC = state.ToConsider.Add(path, name)
            { state with ToConsider = tC }

    let withTopLevelName name state =
        { state with ToResolve = state.ToResolve.Add name }

    let withRelativeName dir (name: E.RelativeName) state =
        withFileToConsider (Path.Combine(dir, name.Text + ".d.ts")) None state

    let withExternImports dir dsf state =
        findExternImports dsf
        |> Seq.fold (fun st x ->
            match x with
            | E.Relative name ->
                withRelativeName dir name state
            | E.TopLevel name ->
                withTopLevelName name state)
            state

    let withRefComment (dir: FilePath) comment state =
        let p = Path.GetFullPath(Path.Combine(dir, comment))
        withFileToConsider p None state

    let withRefComments (dir: FilePath) comments state =
        Seq.fold (fun st c -> withRefComment dir c st) state comments

    let parseFile topName (path: FilePath) (state: State) =
        let path = Path.GetFullPath(path)
        if state.Parsed.ContainsKey(path) then state else
            let info = FileInfo(path)
            if not info.Exists then
                warn (Missing path) state
            elif info.FullName.EndsWith(".d.ts") |> not then
                warn (UnknownFileType path) state
            else
                match P.ParseFile state.NameBuilder path with
                | P.ParseFailed err ->
                    warn (NoParse (path, err)) state
                | P.ParseOk (dsf, comments) ->
                    let dir = Path.GetDirectoryName(path)
                    let sourceFile = SourceFile(path, dsf, ?name = topName)
                    { state with Parsed = state.Parsed.Add(path, sourceFile) }
                    |> withRefComments dir comments
                    |> withExternImports dir dsf

    let Resolve cfg =
        let state = { init () with ToConsider = Map [for f in cfg.StartFiles -> (f, None)] }
        let rec loop state =
            if state.ToConsider.IsEmpty && state.ToResolve.IsEmpty then state else
                let state =
                    state.ToResolve
                    |> Seq.fold (fun state name ->
                        match cfg.Resolver.Find name with
                        | None -> warn (Unresolved name) state
                        | Some p -> withFileToConsider p (Some name) state)
                        { state with ToResolve = Set.empty }
                let state =
                    state.ToConsider
                    |> Map.fold (fun state path topName ->
                        let topName =
                            match topName with
                            | None -> cfg.Resolver.ComputeName(path)
                            | r -> r
                        parseFile topName path state)
                        { state with ToConsider = Map.empty }
                loop state
        let state = loop state
        Result([| for kv in state.Parsed -> kv.Value |], Seq.toArray state.Warnings)

