#load "tools/includes.fsx"
#load "main/VS/VisualStudio.fsx"
#r "System.IO.Compression"
#r "System.IO.Compression.FileSystem"

open System
open System.IO
open System.IO.Compression
open System.Net
open IntelliFactory.Build
module VSI = IntelliFactory.WebSharper.VisualStudioIntegration

module Config =
    let PackageId = "WebSharper.TypeScript"
    let NumericVersion = Version("2.5.0.0")
    let VersionSuffix = Some "alpha"
    let PackageVerion = "2.5"
    let Company = "IntelliFactory"
    let Description = "Supports cross-compiling TypeScript definition files to WebSharper libraries"
    let LicenseUrl = "http://websharper.com/licensing"
    let Tags = ["Web"; "JavaScript"; "F#"; "TypeScript"]
    let Website = "http://bitbucket.org/IntelliFactory/websharper.typescript"

let bt =
    BuildTool().PackageId(Config.PackageId, "2.5-alpha")
    |> fun bt -> bt.WithFramework(bt.Framework.Net40)

let downloadContrib () =
    let url = "http://github.com/borisyankov/DefinitelyTyped/archive/master.zip"
    if Directory.Exists("contrib") |> not then
        Directory.CreateDirectory("contrib") |> ignore
        let file = "contrib/DefinitelyTyped.zip"
        use c = new WebClient()
        c.DownloadFile(url, file)
        ZipFile.ExtractToDirectory(file, "contrib")

let wsPaths () =
    [
        "tools/net40/Mono.Cecil.dll"
        "tools/net40/Mono.Cecil.Mdb.dll"
        "tools/net40/Mono.Cecil.Pdb.dll"
        "tools/net40/IntelliFactory.Core.dll"
        "tools/net40/IntelliFactory.JavaScript.dll"
        "tools/net40/IntelliFactory.WebSharper.Core.dll"
        "tools/net40/IntelliFactory.WebSharper.Compiler.dll"
    ]

let main () =
    (bt.WebSharper.Library("IntelliFactory.WebSharper.TypeScript")
    |> FSharpConfig.BaseDir.Custom "main")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.NuGet("FParsec").Reference()
                bt.Reference.NuGet("WebSharper").At(wsPaths()).Reference()
            ])
    |> BuildConfig.KeyFile.Custom None

let msbuildTasks main =
    (bt.FSharp.Library("IntelliFactory.WebSharper.TypeScript.MSBuild")
    |> FSharpConfig.BaseDir.Custom "msbuild-tasks")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(main)
                r.Assembly "Microsoft.Build"
                r.Assembly "Microsoft.Build.Engine"
                r.Assembly "Microsoft.Build.Framework"
                r.Assembly "Microsoft.Build.Tasks.v4.0"
                r.Assembly "Microsoft.Build.Utilities.v4.0"
            ])
    |> BuildConfig.KeyFile.Custom None

let exe msb main =
    (bt.FSharp.ConsoleExecutable("WebSharper.TSC")
    |> FSharpConfig.BaseDir.Custom "exe")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(msb)
                r.Project(main)
                r.NuGet("FParsec").Reference()
                bt.Reference.NuGet("WebSharper").At(wsPaths()).Reference()
            ])
    |> BuildConfig.KeyFile.Custom None



//let typeProvider () =
//    (bt.FSharp.Library("IntelliFactory.WebSharper.TypeScript.TypeProvider")
//    |> FSharpConfig.BaseDir.Custom "tp")
//        .SourcesFromProject()
//        .References(fun r ->
//            [
//                r.Project(main)
//                r.Assembly("FSharp.Data.TypeProviders")
//            ])

let prepareTests () =
    let fparsec = bt.Reference.NuGet("FParsec").Reference()
    let refs =
        bt.ResolveReferences bt.Framework.Net40 [
            fparsec
            bt.Reference.NuGet("WebSharper").At(wsPaths()).Reference()
        ]
    bt.FSharp.ExecuteScript("scripts/prepareTests.fsx", refs)

let runTests () =
    let refs =
        bt.ResolveReferences bt.Framework.Net40 [
            bt.Reference.NuGet("Fuchu").Reference()
        ]
    bt.FSharp.ExecuteScript("scripts/runTests.fsx", refs)

let tests main =
    (bt.WebSharper.HtmlWebsite("IntelliFactory.WebSharper.TypeScript.Tests")
    |> FSharpConfig.BaseDir.Custom "tests")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Assembly("FSharp.Data.TypeProviders")
                r.Project(main)
                r.File(Path.GetFullPath("build/Tests.dll"))
                r.NuGet("FParsec").Reference()
                // r.Project(typeProvider)
                r.NuGet("Fuchu").Reference()
            ])
    |> BuildConfig.KeyFile.Custom None

let configureVSI (mainPkg: NuGetPackageBuilder) (libPkg: NuGetPackageBuilder) : VSI.Config =
    let root = __SOURCE_DIRECTORY__
    let packages =
        [
            (Config.PackageId, mainPkg)
            ("WebSharper.TypeScript.Lib", libPkg)
        ]
    let nuPkg = mainPkg
    let nupkgPath = nuPkg.GetComputedFileName()
    let vsixPath = Path.ChangeExtension(nupkgPath, ".vsix")
    {
        NuPkgs =
            [
                for (id, p) in packages ->
                    {
                        Path = p.GetComputedFileName()
                        Id = id
                    }]
        RootPath = root
        VsixPath = vsixPath
    }

let libPkg libPath =
    let nuPkg = bt.NuGet.CreatePackage()
    let nuPkg =
        nuPkg.Configure(fun x ->
            {
                x with
                    Description = "WebSharper-compiled TypeScript standard libirary (`lib.d.ts`)"
                    ProjectUrl = Some Config.Website
                    LicenseUrl = Some Config.LicenseUrl
                    Id = "WebSharper.TypeScript.Lib"
                    OutputPath = x.OutputPath.Replace(Config.PackageId, "WebSharper.TypeScript.Lib")
            })
    nuPkg.AddNuGetExportingProject {
        new INuGetExportingProject with
            member p.NuGetFiles =
                Seq.singleton {
                    new INuGetFile with
                        member x.Read() = File.OpenRead(libPath) :> _
                        member x.TargetPath = "/lib/net40/" + Path.GetFileName(libPath)
                }
    }

let mainPkg main libPkg msb =
    let nuPkg =
        bt.NuGet.CreatePackage()
            .Configure(fun x ->
                {
                    x with
                        Description = Config.Description
                        ProjectUrl = Some Config.Website
                        LicenseUrl = Some Config.LicenseUrl
                })
            // .AddNuGetExportingProject(main)
            .AddNuGetExportingProject(exe main msb)
    nuPkg.AddNuGetExportingProject {
        new INuGetExportingProject with
            member p.NuGetFiles =
                seq {
                    let cfg = configureVSI nuPkg libPkg
                    yield! VSI.BuildContents cfg
                    yield {
                        new INuGetFile with
                            member x.Read() = File.OpenRead(typedefof<list<_>>.Assembly.Location) :> _
                            member x.TargetPath = "/tools/net40/FSharp.Core.dll"
                    }
                    yield {
                        new INuGetFile with
                            member x.Read() = File.OpenRead("exe/App.config") :> _
                            member x.TargetPath = "/tools/net40/WebSharper.TSC.exe.config"
                    }
                }
    }

let buildVsix nuPkg libPkg =
    configureVSI nuPkg libPkg
    |> VSI.BuildVsixFile

let buildLib () =
    let fparsec = bt.Reference.NuGet("FParsec").Reference()
    let refs =
        bt.ResolveReferences bt.Framework.Net40 [
            fparsec
            bt.Reference.NuGet("WebSharper").At(wsPaths()).Reference()
        ]
    bt.FSharp.ExecuteScript("scripts/buildLib.fsx", refs)

let build () =
    downloadContrib ()
    let main = main ()
    let msb = msbuildTasks main
    bt.Solution [ main; msb; exe msb main ] |> bt.Dispatch
    prepareTests ()
    let tests = tests main
    bt.Solution [ tests ] |> bt.Dispatch
    runTests ()
    buildLib ()
    let libPkg = libPkg "build/net40/IntelliFactory.WebSharper.TypeScript.Lib.dll"
    let mainPkg = mainPkg main libPkg msb
    bt.Solution [ libPkg; mainPkg ] |> bt.Dispatch
    buildVsix mainPkg libPkg

build ()
