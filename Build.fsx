#load "tools/includes.fsx"

open IntelliFactory.Build

let bt = BuildTool().PackageId("WebSharper.TypeScript", "2.5")

let main =
    (bt.WebSharper.Library("IntelliFactory.WebSharper.TypeScript")
    |> FSharpConfig.BaseDir.Custom "main")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.NuGet("FParsec").Reference()
            ])

let typeProvider =
    (bt.FSharp.Library("IntelliFactory.WebSharper.TypeScript.TypeProvider")
    |> FSharpConfig.BaseDir.Custom "tp")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(main)
                r.Assembly("FSharp.Data.TypeProviders")
            ])

let tests =
    (bt.FSharp.ConsoleExecutable("IntelliFactory.WebSharper.TypeScript.Tests")
    |> FSharpConfig.BaseDir.Custom "tests")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(main)
                r.NuGet("Fuchu").Reference()
            ])

let testSite =
    (bt.WebSharper.HtmlWebsite("IntelliFactory.WebSharper.TypeScript.TestSite")
    |> FSharpConfig.BaseDir.Custom "web")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Assembly("FSharp.Data.TypeProviders")
                r.Project(main)
                r.Project(typeProvider)
            ])

bt.Solution [
    main
    typeProvider
    tests
    testSite
]
|> bt.Dispatch
