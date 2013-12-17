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
            TopLevelClassName : string
            TypeScriptDeclarationFiles : seq<FilePath>
        }

    [<Sealed>]
    type CompiledAssembly(cfg: Config, bytes: byte[]) =
        member a.GetBytes() = Array.copy bytes
        member a.TopLevelClassName = cfg.TopLevelClassName

    let GetSourceFileSet cfg =
        {
            SFD.Configure(cfg.TypeScriptDeclarationFiles) with
                Resolver =
                    SFD.Resolver.Create
                        (
                            (fun _ -> failwith "Resolution not yet supported"), // TODO
                            (fun _ -> failwith "Resolution not yet supported") // TODO)
                        )
        }
        |> SFD.Resolve

    let AnalyzeSourceFiles (sourceFiles: SFD.Result) =
        Analysis.Analyze {
            SourceFiles = sourceFiles.SourceFiles
        }

    let MangleNames (out: Analysis.Output) =
        Naming.Do out

    let EmitAssembly (cfg: Config) top =
        ReflectEmit.ConstructAssembly {
            AssemblyName = cfg.AssemblyName
            TopLevelClassName = cfg.TopLevelClassName
            TopModule = top
        }

    let Compile (cfg: Config) =
        let bytes =
            GetSourceFileSet cfg
            |> AnalyzeSourceFiles
            |> MangleNames
            |> EmitAssembly cfg
        CompiledAssembly(cfg, bytes)

    let GuessAssemblyName (topLevelClassName: string) =
        match topLevelClassName.Split([| '.' |], StringSplitOptions.RemoveEmptyEntries) with
        | x when x.Length > 1 ->
            Array.sub x 0 (x.Length - 1)
            |> String.concat "."
        | _ -> topLevelClassName

    let Configure (topLevelClassName: string) (paths: seq<FilePath>) =
        {
            AssemblyName = GuessAssemblyName topLevelClassName
            TopLevelClassName = topLevelClassName
            TypeScriptDeclarationFiles = paths
        }
