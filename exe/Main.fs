// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

namespace WebSharper.TSC

open System
open System.IO
open System.Reflection
open System.Text
open System.Text.RegularExpressions
module C = IntelliFactory.WebSharper.TypeScript.Compiler

module internal Main =

    type ParseState =
        {
            AssemblyName : option<string>
            Errors : list<string>
            FilePaths : list<string>
            References : list<C.ReferenceAssembly>
            TempDir : option<string>
            TopLevelClassName : option<string>
            OutputPath : option<string>
            Verbosity : C.Level
        }

    let InitialState =
        {
            AssemblyName = None
            Errors = []
            FilePaths = []
            References = []
            TempDir = None
            TopLevelClassName = None
            OutputPath = None
            Verbosity = C.Level.Warn
        }

    let Error st fmt =
        Printf.ksprintf (fun err -> { st with Errors = err :: st.Errors }) fmt

    let PrintUsage (st: ParseState) =
        stderr.WriteLine("WebSharper.TSC.exe: Cross-compilers TypeScript .d.ts files for WebSharper")
        if st.Errors.IsEmpty |> not then
            stderr.WriteLine()
            stderr.WriteLine("Errors: ")
            for err in List.rev st.Errors do
                stderr.WriteLine(err)
        stderr.WriteLine()
        stderr.WriteLine("Usage: WebSharper.TSC.exe -c ClassName -o X.dll [FLAGS] Input.d.ts ...")
        stderr.WriteLine()
        stderr.WriteLine("Flags: ")
        stderr.WriteLine()
        stderr.WriteLine("-a <name>     sets the output AssemblyName")
        stderr.WriteLine("-c <name>     sets the fully qualified top-level class name ")
        stderr.WriteLine("-o <path>     sets the output path")
        stderr.WriteLine("-r <path>     adds a DLL reference")
        stderr.WriteLine("-log <level>  sets the verbosity (critical, error, warn, info, verbose)")
        stderr.WriteLine("-tmp <dir>    sets the temporary directory to use")

    let ParseOptions args =
        let rec loop st args =
            match args with
            | "-o" :: path :: rest ->
                let st =
                    match st.OutputPath with
                    | None -> { st with OutputPath = Some path }
                    | Some _ -> Error st "Duplicate flag: -o"
                loop st rest
            | "-a" :: name :: rest ->
                let st =
                    match st.AssemblyName with
                    | None ->
                        try
                            let n = string (AssemblyName(name))
                            { st with AssemblyName = Some n }
                        with e ->
                            Error st "Invalid AssemblyName: [%s]. Details: %O" name e
                    | Some _ -> Error st "Duplicate flag: -a"
                loop st rest
            | "-log" :: l :: rest ->
                let level =
                    match l with
                    | "critical" -> Some C.Level.Critical
                    | "error" -> Some C.Level.Error
                    | "info" -> Some C.Level.Info
                    | "verbose" -> Some C.Level.Verbose
                    | "warn" -> Some C.Level.Warn
                    | _ -> None
                let st =
                    match level with
                    | None -> Error st "Invalid logging level: %s" l
                    | Some level -> { st with Verbosity = level }
                loop st rest
            | "-tmp" :: tmp :: rest ->
                let st =
                    match st.TempDir with
                    | None ->
                        if Directory.Exists(tmp) then
                            { st with TempDir = Some tmp }
                        else Error st "Directory does not exist: %s" tmp
                    | Some _ -> Error st "Duplicate flag: -tmp"
                loop st rest
            | "-c" :: name :: rest ->
                let st =
                    match st.TopLevelClassName with
                    | None -> { st with TopLevelClassName = Some name }
                    | Some _ -> Error st "Duplicate flag: -c"
                loop st rest
            | "-r" :: path :: rest ->
                let st =
                    if File.Exists(path) then
                        let asm = C.ReferenceAssembly.File path
                        { st with References = asm :: st.References }
                    else Error st "File does not exist: %s" path
                loop st rest
            | path :: rest ->
                let st =
                    if File.Exists(path) then
                        { st with FilePaths = path :: st.FilePaths }
                    else Error st "File does not exist: %s" path
                loop st rest
            | [] ->
                let check msg cond st =
                    if cond st then Error st "Flag -c (TopLevelClassName) is required" else st
                let st =
                    st
                    |> check "Flag -c (TopLevelClassName) is required" (fun st -> st.TopLevelClassName.IsNone)
                    |> check "One or more .d.ts paths are required" (fun st -> st.FilePaths.IsEmpty)
                    |> check "Flag -o (OutputPath) is required" (fun st -> st.OutputPath.IsNone)
                if not st.Errors.IsEmpty then PrintUsage st; None else
                    let cfg =
                        {
                            C.Options.Create(st.TopLevelClassName.Value, st.FilePaths) with
                                References = st.References
                                Verbosity = st.Verbosity
                        }
                    let cfg =
                        match st.TempDir with
                        | Some tmp -> { cfg with TemporaryFolder = tmp }
                        | None -> cfg
                    let cfg =
                        match st.AssemblyName with
                        | Some name -> { cfg with AssemblyName = name }
                        | None -> cfg
                    Some (st, cfg)
        loop InitialState args

    let WriteFile path bytes =
        let dir = DirectoryInfo(Path.GetDirectoryName(path))
        if dir.Exists |> not then
            dir.Create() |> ignore
        File.WriteAllBytes(path, bytes)

    let Compile st cfg =
        let result = C.Compile cfg
        for msg in result.Messages do
            stdout.WriteLine(msg)
        match result.CompiledAssembly with
        | None -> stderr.WriteLine("Compilation failed"); 1
        | Some assem ->
            let bytes = assem.GetBytes()
            let path = st.OutputPath.Value
            WriteFile path bytes
            stdout.WriteLine("Written: {0}", path); 0

    [<EntryPoint>]
    let Start args =
        let args = Array.toList args
        match ParseOptions args with
        | None -> 1
        | Some (st, cfg) -> Compile st cfg
