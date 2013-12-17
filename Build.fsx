#load "tools/includes.fsx"

open IntelliFactory.Build

let bt = BuildTool().PackageId("WebSharper.TypeScript", "2.5")

let main =
    bt.FSharp.Library("IntelliFactory.WebSharper.TypeScript")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.NuGet("FParsec").Reference()
            ])

let typeProvider =
    bt.FSharp.ConsoleExecutable("IntelliFactory.WebSharper.TypeScript.TypeProvider")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(main)
                r.Assembly("FSharp.Data.TypeProviders")
            ])

let tests =
    bt.FSharp.ConsoleExecutable("IntelliFactory.WebSharper.TypeScript.Tests")
        .SourcesFromProject()
        .References(fun r ->
            [
                r.Project(main)
                r.NuGet("Fuchu").Reference()
            ])

bt.Solution [
    main
    typeProvider
    tests
]
|> bt.Dispatch
