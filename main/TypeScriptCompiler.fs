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

open System
module SFD = SourceFileDependencies

module TypeScriptCompiler =

    type Config =
        {
            AssemblyName : string
            References : seq<Assembly>
            TemporaryFolder : string
            TopLevelClassName : string
            TypeScriptDeclarationFiles : seq<FilePath>
            Verbosity : Logging.Level
        }

    [<Sealed>]
    type CompiledAssembly(cfg: Config, bytes: byte[]) =
        member a.GetBytes() = Array.copy bytes
        member a.TopLevelClassName = cfg.TopLevelClassName

    let GetSourceFileSet logger cfg =
        {
            SFD.Configure logger cfg.TypeScriptDeclarationFiles with
                Resolver = SFD.Resolver.Failure // TODO
        }
        |> SFD.Resolve

    let AnalyzeSourceFiles refs logger (sourceFiles: SFD.Result) =
        Analysis.Analyze {
            MetadataTable =
                refs
                |> Seq.distinct
                |> Seq.choose Metadata.Table.TryParseAssembly
                |> Metadata.Table.Union
            Logger = logger
            SourceFiles = sourceFiles.SourceFiles
        }

    let MangleNames (out: Analysis.Output) =
        Naming.Do out

    let EmitAssembly cfg top =
        ReflectEmit.ConstructAssembly {
            AssemblyName = cfg.AssemblyName
            TemporaryFolder = cfg.TemporaryFolder
            TopLevelClassName = cfg.TopLevelClassName
            TopModule = top
        }

    [<Sealed>]
    type Result(msgs: seq<Logging.Message>, ?assem: CompiledAssembly) =
        member r.CompiledAssembly = assem
        member r.Messages = msgs

    let Run (l: Logger) verb : (unit -> 'T) -> option<'T> =
        match verb with
        | Logging.Verbose ->
            fun f -> Some (f ())
        | _ ->
            fun f -> try Some (f ()) with e -> l.Exception(e); None

    let Compile cfg =
        let logger = Logger(cfg.Verbosity)
        let assem =
            Run logger cfg.Verbosity <| fun () ->
                let bytes =
                    GetSourceFileSet logger cfg
                    |> AnalyzeSourceFiles cfg.References logger
                    |> MangleNames
                    |> EmitAssembly cfg
                CompiledAssembly(cfg, bytes)
        Result(logger.All, ?assem = assem)

    let GuessAssemblyName (topLevelClassName: string) =
        match topLevelClassName.Split([| '.' |], StringSplitOptions.RemoveEmptyEntries) with
        | x when x.Length > 1 ->
            Array.sub x 0 (x.Length - 1)
            |> String.concat "."
        | _ -> topLevelClassName

    let Configure (topLevelClassName: string) (paths: seq<FilePath>) =
        {
            AssemblyName = GuessAssemblyName topLevelClassName
            References = Seq.empty
            TemporaryFolder = Path.GetTempPath()
            TopLevelClassName = topLevelClassName
            TypeScriptDeclarationFiles = paths
            Verbosity = Logging.Warn
        }
