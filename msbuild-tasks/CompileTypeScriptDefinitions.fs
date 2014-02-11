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
        {
            AssemblyName = assemblyName
            References = refs
            TemporaryFolder = tempFolder
            TopLevelClassName = topLevelClassName
            TypeScriptDeclarationFiles = files
            Verbosity = Logging.Verbose
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
        let fileName = AssemblyName(t.AssemblyName).Name + ".dll"
        let fullPath = Path.Combine(t.MSBuildProjectDirectory, t.OutputPath, fileName)
        let dir = DirectoryInfo(Path.GetDirectoryName(fullPath))
        if not <| dir.Exists then
            dir.Create()
        File.WriteAllBytes(fullPath, r.GetBytes())
        t.Log.LogMessage(MessageImportance.Low, "Written: {0}", fullPath)

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

    [<Required>]
    member val MSBuildProjectDirectory : string = null with get, set

    [<Required>]
    member val OutputPath : string = null with get, set

    member val ReferencePaths : ITaskItem [] = Array.empty with get, set

    member val TempDirectory : string = null with get, set

    member val TopLevelClassName : string = null with get, set

[<Sealed>]
type CleanTypeScriptDefinitions() =
    inherit Task()

    [<Required>]
    member val AssemblyName : string = null with get, set

    [<Required>]
    member val MSBuildProjectDirectory : string = null with get, set

    [<Required>]
    member val OutputPath : string = null with get, set

    override t.Execute() =
        let fileName = AssemblyName(t.AssemblyName).Name + ".dll"
        let fullPath = Path.Combine(t.MSBuildProjectDirectory, t.OutputPath, fileName)
        let info = FileInfo(fullPath)
        if info.Exists then
            info.Delete()
        true
