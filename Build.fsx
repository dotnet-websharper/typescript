#load "tools/includes.fsx"
#r "System.IO.Compression"
#r "System.IO.Compression.FileSystem"

open System
open System.IO
open System.IO.Compression
open System.Net
open IntelliFactory.Build

let bt = BuildTool().PackageId("WebSharper.TypeScript", "2.5")

let downloadContrib () =
    let url = "http://github.com/borisyankov/DefinitelyTyped/archive/master.zip"
    if Directory.Exists("contrib") |> not then
        Directory.CreateDirectory("contrib") |> ignore
        let file = "contrib/DefinitelyTyped.zip"
        use c = new WebClient()
        c.DownloadFile(url, file)
        ZipFile.ExtractToDirectory(file, "contrib")

downloadContrib ()

let wsPaths =
    [
        "tools/net45/Mono.Cecil.dll"
        "tools/net45/IntelliFactory.Core.dll"
        "tools/net45/IntelliFactory.JavaScript.dll"
        "tools/net45/IntelliFactory.WebSharper.Core.dll"
        "tools/net45/IntelliFactory.WebSharper.Compiler.dll"
    ]

let main =
    (bt.WebSharper.Library("IntelliFactory.WebSharper.TypeScript")
    |> FSharpConfig.BaseDir.Custom "main")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.NuGet("FParsec").Reference()
                bt.Reference.NuGet("WebSharper").At(wsPaths).Reference()
            ])
    |> BuildConfig.KeyFile.Custom None

//let typeProvider =
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
        bt.ResolveReferences bt.Framework.Net45 [
            fparsec
            bt.Reference.NuGet("WebSharper").At(wsPaths).Reference()
        ]
    bt.FSharp.ExecuteScript("tests/prepare.fsx", refs)

let runTests () =
    let refs =
        bt.ResolveReferences bt.Framework.Net45 [
            bt.Reference.NuGet("Fuchu").Reference()
        ]
    bt.FSharp.ExecuteScript("tests/runTests.fsx", refs)

let tests =
    (bt.WebSharper.HtmlWebsite("IntelliFactory.WebSharper.TypeScript.Tests")
    |> FSharpConfig.BaseDir.Custom "tests")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Assembly("FSharp.Data.TypeProviders")
                r.Project(main)
                r.File(Path.GetFullPath("build/Tests.dll"))
                // r.Project(typeProvider)
                r.NuGet("Fuchu").Reference()
            ])
    |> BuildConfig.KeyFile.Custom None

bt.Solution [ main ] |> bt.Dispatch
prepareTests ()
bt.Solution [ tests ] |> bt.Dispatch
runTests ()
