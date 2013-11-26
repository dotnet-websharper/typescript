#load "tools/includes.fsx"

open IntelliFactory.Build

let bt = BuildTool().PackageId("IntelliFactory.TypeScript", "0.1")

let lib1 =
    bt.FSharp.Library("IntelliFactory.TypeScript")
        .SourcesFromProject()
        .References(fun r ->
            let paths =
                [
                    "tools/net45/IntelliFactory.WebSharper.Core.dll"
                    "tools/net45/IntelliFactory.JavaScript.dll"
                ]
            [
                r.NuGet("IntelliFactory.Parsec").Reference()
                r.NuGet("WebSharper").At(paths).Reference()
            ])

let lib2 =
    bt.FSharp.Library("IntelliFactory.TypeScript.TypeProvider")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Assembly("FSharp.Data.TypeProviders")
                r.Project(lib1)
            ])

let sln =
    bt.Solution [
        lib1
        lib2
        bt.NuGet.CreatePackage()
            .Add(lib1)
            .Add(lib2)
            .Configure(fun proj ->
                {
                    proj with
                        Authors = ["IntelliFactory"]
                        Description = "WebSharper-compatible TypeScript analyzer and bindings compiler"
                        LicenseUrl = Some "http://websharper.com/licensing"
                        ProjectUrl = Some "http://bitbucket.org/IntelliFactory/typescript"
                })
    ]

bt.Dispatch sln
