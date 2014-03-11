namespace IntelliFactory.WebSharper.TypeScript.MSBuild

open System
open System.IO
open System.Reflection
open Microsoft.Build
open Microsoft.Build.Framework
open Microsoft.Build.Utilities
open IntelliFactory.WebSharper.TypeScript
module C = IntelliFactory.WebSharper.TypeScript.TypeScriptCompiler

[<Sealed>]
type BuildTypeScriptDefinitions() =
    inherit Task()

    member t.Configure() : C.Config =
        let assemblyName =
            match t.AssemblyName with
            | null | "" -> "Assembly"
            | n -> n
        let refs =
            [|
                for r in t.ReferencePaths ->
                    C.ReferenceAssembly.File r.ItemSpec
            |]
        let tempFolder =
            match t.TempDirectory with
            | null | "" -> Path.GetTempPath()
            | t -> t
        let topLevelClassName =
            match t.TopLevelClassName with
            | null | "" -> assemblyName
            | n -> n
        let files =
            [|
                for f in t.Compile ->
                    f.ItemSpec
            |]
        let resources =
            [|
                for x in t.EmbeddedResources ->
                    EmbeddedResource.FromFile(x.ItemSpec)
            |]
        let wsResources =
            [|
                for x in t.EmbeddedResources do
                    match x.GetMetadata("ClassName") with
                    | null | "" -> ()
                    | name ->
                        let path = Path.GetFileName(x.ItemSpec)
                        yield WebSharperResource.Create(name, path)
            |]
        {
            AssemblyName = assemblyName
            EmbeddedResources = resources
            References = refs
            TemporaryFolder = tempFolder
            TopLevelClassName = topLevelClassName
            TypeScriptDeclarationFiles = files
            Verbosity = Logging.Verbose
            WebSharperResources = wsResources
        }

    member t.Report(msgs: seq<Logging.Message>) =
        for m in msgs do
            match m.Level with
            | Logging.Critical | Logging.Error ->
                t.Log.LogError(m.Text)
            | Logging.Warn ->
                t.Log.LogWarning(m.Text)
            | Logging.Level.Info ->
                t.Log.LogMessage(MessageImportance.Normal, m.Text)
            | Logging.Level.Verbose ->
                t.Log.LogMessage(MessageImportance.Low, m.Text)

    member t.Write(r: C.CompiledAssembly) =
        match t.IntermediateAssembly with
        | [| path |] ->
            let fullPath = Path.Combine(t.MSBuildProjectDirectory, path.ItemSpec)
            let dir = DirectoryInfo(Path.GetDirectoryName(fullPath))
            if not <| dir.Exists then
                dir.Create()
            File.WriteAllBytes(fullPath, r.GetBytes())
            t.Log.LogMessage(MessageImportance.Low, "Written: {0}", fullPath)
        | _ ->
            t.Log.LogError("Invalid IntermediateAssembly")

    override t.Execute() =
        let cfg = t.Configure()
        let result = C.Compile cfg
        t.Report(result.Messages)
        match result.CompiledAssembly with
        | Some result -> t.Write(result); true
        | None -> false

    [<Required>]
    member val AssemblyName : string = null with get, set

    member val Compile : ITaskItem [] = Array.empty with get, set

    member val EmbeddedResources : ITaskItem [] = Array.empty with get, set

    [<Required>]
    member val IntermediateAssembly : ITaskItem [] = Array.empty with get, set

    [<Required>]
    member val MSBuildProjectDirectory : string = null with get, set

    member val ReferencePaths : ITaskItem [] = Array.empty with get, set

    member val TempDirectory : string = null with get, set

    member val TopLevelClassName : string = null with get, set
