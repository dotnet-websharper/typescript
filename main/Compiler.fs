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
    type WebSharperResource = global.IntelliFactory.WebSharper.TypeScript.WebSharperResource

    type ReferenceAssembly =
        | AFile of string
        | ARaw of byte []

        static member File(path) = AFile path
        static member Raw(bytes) = ARaw bytes

    let LoadAndInstallReferences (refs: seq<ReferenceAssembly>) =
        // TODO: perhaps complain on duplicate refs.
        let all =
            dict <| seq {
                for a in refs do
                    let assem =
                        match a with
                        | AFile f -> Assembly.LoadFile(f)
                        | ARaw bytes -> Assembly.Load(bytes)
                    yield (assem.GetName().Name, assem)
            }
        AppDomain.CurrentDomain.add_AssemblyResolve(fun obj ev ->
            let name = AssemblyName(ev.Name).Name
            let mutable res = Unchecked.defaultof<_>
            if all.TryGetValue(name, &res) then res else null)
        [| for KeyValue (_, v) in all -> v |]

    type Config =
        {
            AssemblyName : string
            EmbeddedResources : seq<EmbeddedResource>
            References : seq<ReferenceAssembly>
            TemporaryFolder : string
            TopLevelClassName : string
            TypeScriptDeclarationFiles : seq<FilePath>
            Verbosity : Logging.Level
            WebSharperResources : seq<WebSharperResource>
        }

    [<Sealed>]
    type CompiledAssembly(cfg: Config, bytes: byte[]) =
        member a.Config = cfg
        member a.GetBytes() = Array.copy bytes
        member a.AssemblyName = cfg.AssemblyName
        member a.TopLevelClassName = cfg.TopLevelClassName

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

    let MangleNames (out: Analysis.Output) =
        Naming.Do out

    let EmitAssembly cfg top =
        ReflectEmit.ConstructAssembly {
            AssemblyName = cfg.AssemblyName
            EmbeddedResources = cfg.EmbeddedResources
            TemporaryFolder = cfg.TemporaryFolder
            TopLevelClassName = cfg.TopLevelClassName
            TopModule = top
            WebSharperResources = cfg.WebSharperResources
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

        let LoggingLevelPickler =
            Pickler.DefSum (fun x k1 k2 k3 k4 k5 ->
                match x with
                | Logging.Verbose -> k1 ()
                | Logging.Info -> k2 ()
                | Logging.Warn -> k3 ()
                | Logging.Error -> k4 ()
                | Logging.Critical -> k5 ())
            ^ Pickler.Variant Logging.Verbose
            ^ Pickler.Variant Logging.Info
            ^ Pickler.Variant Logging.Warn
            ^ Pickler.Variant Logging.Error
            ^ Pickler.LastVariant Logging.Critical

        let ReferenceAssemblyPickler =
            Pickler.DefSum (fun x k1 k2 ->
                match x with
                | AFile f -> k1 f
                | ARaw b -> k2 b)
            ^ Pickler.Case AFile Pickler.String
            ^ Pickler.LastCase ARaw Pickler.Bytes


        let ConfigPickler =
            Pickler.DefProduct (fun x1 x2 x3 x4 x5 x6 x7 x8 ->
                {
                    AssemblyName = x1
                    EmbeddedResources = x2
                    References = x3
                    TemporaryFolder = x4
                    TopLevelClassName = x5
                    TypeScriptDeclarationFiles = x6
                    Verbosity = x7
                    WebSharperResources = x8
                })
            ^ Pickler.Field (fun p -> p.AssemblyName) Pickler.String
            ^ Pickler.Field (fun p -> p.EmbeddedResources) (Pickler.Seq EmbeddedResource.Pickler)
            ^ Pickler.Field (fun p -> p.References) (Pickler.Seq ReferenceAssemblyPickler)
            ^ Pickler.Field (fun p -> p.TemporaryFolder) Pickler.String
            ^ Pickler.Field (fun p -> p.TopLevelClassName) Pickler.String
            ^ Pickler.Field (fun p -> p.TypeScriptDeclarationFiles) (Pickler.Seq Pickler.String)
            ^ Pickler.Field (fun p -> p.Verbosity) LoggingLevelPickler
            ^ Pickler.Field (fun p -> p.WebSharperResources) (Pickler.Seq WebSharperResource.Pickler)
            ^ Pickler.EndProduct ()

        let CompiledAssemblyPickler =
            Pickler.DefProduct (fun cfg bytes -> CompiledAssembly(cfg, bytes))
            ^ Pickler.Field (fun (a: CompiledAssembly) -> a.Config) ConfigPickler
            ^ Pickler.Field (fun (a: CompiledAssembly) -> a.GetBytes()) Pickler.Bytes
            ^ Pickler.EndProduct()

        let MessagePickler =
            Pickler.DefProduct (fun l t -> Logging.Message.Create(l, t))
            ^ Pickler.Field (fun (m: Logging.Message) -> m.Level) LoggingLevelPickler
            ^ Pickler.Field (fun (m: Logging.Message) -> m.Text) Pickler.String
            ^ Pickler.EndProduct()

        let ResultPickler : Pickler.T<Result> =
            Pickler.DefProduct (fun a b -> Result(a, ?assem = b))
            ^ Pickler.Field (fun r -> r.Messages) (Pickler.Seq MessagePickler)
            ^ Pickler.Field (fun r -> r.CompiledAssembly) (Pickler.Option CompiledAssemblyPickler)
            ^ Pickler.EndProduct()

    [<Sealed>]
    type CompileTransform() =
        interface AppDomains.ITransform<Config,Result> with
            member tr.Do(cfg: Config) =
                let logger = Logger(cfg.Verbosity)
                let builder = Names.NameBuilder.Create()
                let assem =
                    Run logger cfg.Verbosity <| fun () ->
                        let refs = LoadAndInstallReferences cfg.References
                        let bytes =
                            GetSourceFileSet builder logger cfg
                            |> AnalyzeSourceFiles builder refs logger
                            |> MangleNames
                            |> EmitAssembly cfg
                        CompiledAssembly(cfg, bytes)
                Result(logger.All, ?assem = assem)

            member tr.T1 = Pickle.ConfigPickler
            member tr.T2 = Pickle.ResultPickler

    let Compile cfg =
        AppDomains.TransformWithAppDomain AppDomains.MarkType<CompileTransform> cfg

    let GuessAssemblyName (topLevelClassName: string) =
        match topLevelClassName.Split([| '.' |], StringSplitOptions.RemoveEmptyEntries) with
        | x when x.Length > 1 ->
            Array.sub x 0 (x.Length - 1)
            |> String.concat "."
        | _ -> topLevelClassName

    let Configure (topLevelClassName: string) (paths: seq<FilePath>) =
        {
            AssemblyName = GuessAssemblyName topLevelClassName
            EmbeddedResources = Seq.empty
            References = Seq.empty
            TemporaryFolder = Path.GetTempPath()
            TopLevelClassName = topLevelClassName
            TypeScriptDeclarationFiles = paths
            Verbosity = Logging.Warn
            WebSharperResources = Seq.empty
        }
