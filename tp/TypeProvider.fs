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

module C = TypeScriptCompiler

/// Implements a TypeProvider interface to `IntelliFactory.WebSharper.TypeScript`.
[<Sealed>]
[<TypeProvider>]
type TypeProvider(config: TypeProviderConfig) =
    inherit DelegatingProvider(TypeProvider.Create(config))
    static do ProviderUtility.InstallAssemblyResolver()

    static let buildAssembly (config: TypeProviderConfig)
                             (className: string)
                             (typeScriptFile: string) =
        let assembly =
            {
                C.Configure className [typeScriptFile] with
                    TemporaryFolder = config.TemporaryFolder
            }
            |> C.Compile
        match assembly.CompiledAssembly with
        | Some asm -> asm
        | None -> failwith "Failed to compile the generated assembly"

    static member private Create(config: TypeProviderConfig) =
        printfn "Creating TP"
        ProviderUtility.DefineProvider {
            TypeProviderConfig = config
            BuildAssembly = fun tc ->
                printfn "Building assembly"
                let tsFile = tc.ParameterValues.[0] :?> string
                let output =
                    if tc.ParameterValues.Length > 1 then
                        match tc.ParameterValues.[1] with
                        | null -> None
                        | :? string as s -> if s = "" then None else Some s
                        | _ -> None
                    else None
                let tsFile = Path.Combine(config.ResolutionFolder, tsFile)
                let asm = buildAssembly config tc.ClassName tsFile
                let bytes = asm.GetBytes()
                match output with
                | None -> ()
                | Some out ->
                    let path = Path.Combine(config.ResolutionFolder, out)
                    File.WriteAllBytes(path, bytes)
                bytes
            GeneratorParameters =
                [
                    ProviderUtility.RequiredParam("file", typeof<string>)
                    ProviderUtility.OptionalParam("output", typeof<string>, "")
                ]
            GeneratorTypeName = "IntelliFactory.WebSharper.TypeScript.TypeScriptGenerator"
        }
