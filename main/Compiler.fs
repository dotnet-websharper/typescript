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

namespace IntelliFactory.WebSharper.TypeScript

module SFD = SourceFileDependencies

module Compiler =
    type EmbeddedResource = global.IntelliFactory.WebSharper.TypeScript.EmbeddedResource
    type Level = global.IntelliFactory.WebSharper.TypeScript.Logging.Level
    type Message = global.IntelliFactory.WebSharper.TypeScript.Logging.Message
    type Options = global.IntelliFactory.WebSharper.TypeScript.CompilerOptions
    type ReferenceAssembly = global.IntelliFactory.WebSharper.TypeScript.ReferenceAssembly
    type Renaming = global.IntelliFactory.WebSharper.TypeScript.Renaming
    type Root = global.IntelliFactory.WebSharper.TypeScript.Root
    type WebSharperResource = global.IntelliFactory.WebSharper.TypeScript.WebSharperResource

    let LoadAndInstallReferences (refs: seq<ReferenceAssembly>) =
        // TODO: perhaps complain on duplicate refs.
        let all =
            dict <| seq {
                for a in refs do
                    let assem = a.Load()
                    yield (assem.GetName().Name, assem)
            }
        AppDomain.CurrentDomain.add_AssemblyResolve(fun obj ev ->
            let name = AssemblyName(ev.Name).Name
            let mutable res = Unchecked.defaultof<_>
            if all.TryGetValue(name, &res) then res else null)
        [| for KeyValue (_, v) in all -> v |]

    [<Sealed>]
    type CompiledAssembly(cfg: Options, bytes: byte[]) =
        member a.Config = cfg
        member a.GetBytes() = Array.copy bytes
        member a.AssemblyName = cfg.AssemblyName
        member a.Root = cfg.Root

    let GetSourceFileSet nb logger cfg =
        /// TODO: perhaps warn if no input files found.
        {
            SFD.Configure nb logger cfg.TypeScriptDeclarationFiles with
                Resolver = SFD.Resolver.Failure // TODO
        }
        |> SFD.Resolve

    let AnalyzeSourceFiles builder (refs: seq<Assembly>) logger (sourceFiles: SFD.Result) =
        Analysis.Analyze {
            MetadataTable =
                refs
                |> Seq.distinct
                |> Seq.choose (fun a -> Metadata.Table.TryParseAssembly(builder, a))
                |> Metadata.Table.Union
            NameBuilder = builder
            Logger = logger
            SourceFiles = sourceFiles.SourceFiles
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

    module Pickle =

        let inline ( ^ ) f x = f x

        let CompiledAssemblyPickler =
            Pickler.DefProduct (fun cfg bytes -> CompiledAssembly(cfg, bytes))
            ^ Pickler.Field (fun (a: CompiledAssembly) -> a.Config) CompilerOptions.Pickler
            ^ Pickler.Field (fun (a: CompiledAssembly) -> a.GetBytes()) Pickler.Bytes
            ^ Pickler.EndProduct()

        let ResultPickler : Pickler.T<Result> =
            Pickler.DefProduct (fun a b -> Result(a, ?assem = b))
            ^ Pickler.Field (fun r -> r.Messages) (Pickler.Seq Message.Pickler)
            ^ Pickler.Field (fun r -> r.CompiledAssembly) (Pickler.Option CompiledAssemblyPickler)
            ^ Pickler.EndProduct()

    let Emit opts refs top =
        ReflectEmit.ConstructAssembly {
            CompilerOptions = opts
            References = refs
            TopModule = top
        }

    [<Sealed>]
    type CompileTransform() =
        interface AppDomains.ITransform<Options,Result> with
            member tr.Do(cfg: Options) =
                let logger = Logger(cfg.Verbosity)
                let builder = Names.NameBuilder.Create()
                let assem =
                    Run logger cfg.Verbosity <| fun () ->
                        let refs = LoadAndInstallReferences cfg.References
                        let bytes =
                            GetSourceFileSet builder logger cfg
                            |> AnalyzeSourceFiles builder refs logger
                            |> Naming.Do builder cfg.Renaming
                            |> Emit cfg refs
                        CompiledAssembly(cfg, bytes)
                Result(logger.All, ?assem = assem)

            member tr.T1 = CompilerOptions.Pickler
            member tr.T2 = Pickle.ResultPickler

    let Compile cfg =
        AppDomains.TransformWithAppDomain AppDomains.MarkType<CompileTransform> cfg

    let GuessAssemblyName (topLevelClassName: string) =
        match topLevelClassName.Split([| '.' |], StringSplitOptions.RemoveEmptyEntries) with
        | x when x.Length > 1 ->
            Array.sub x 0 (x.Length - 1)
            |> String.concat "."
        | _ -> topLevelClassName
