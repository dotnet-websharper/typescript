#r "packages/IntelliFactory.Build.0.0.6/lib/net40/IntelliFactory.Build.dll"
#r "packages/FAKE.2.1.158-alpha/tools/FakeLib.dll"

open System
open System.IO
open Fake

module B = IntelliFactory.Build.CommonBuildSetup
module X = IntelliFactory.Build.XmlGenerator

let ( +/ ) a b = Path.Combine(a, b)
let RootDir = __SOURCE_DIRECTORY__
let DotBuildDir = RootDir +/ ".build"
let T x f = Target x f; x

module Config =
    let Company = "IntelliFactory"
    let Description = "WebSharper-compatible TypeScript analyzer and bindings compiler"
    let LicenseUrl = "http://websharper.com/licensing"
    let PackageId = "IntelliFactory.TypeScript"
    let Tags = ["WebSharper"; "TypeScript"; "F#"]
    let AssemblyVersion = Version "0.0.0.0"
    let AssemblyFileVersion = Version "0.0.2.0"
    let Version = "0.0.2-alpha"
    let Website = "http://bitbucket.org/IntelliFactory/typescript"

let Metadata =
    let m = B.Metadata.Create()
    m.Author <- Some Config.Company
    m.AssemblyVersion <- Some Config.AssemblyVersion
    m.FileVersion <- Some Config.AssemblyFileVersion
    m.Description <- Some Config.Description
    m.Product <- Some Config.PackageId
    m.Website <- Some Config.Website
    m

let Frameworks = [B.Net40]

let Solution =
    B.Solution.Standard __SOURCE_DIRECTORY__ Metadata [
        B.Project.FSharp "IntelliFactory.TypeScript" Frameworks
        B.Project.FSharp "IntelliFactory.TypeScript.TypeProvider" Frameworks
    ]

let BuildMain = T "BuildMain" Solution.Build
let Build = T "Build" ignore
let Clean = T "Clean" Solution.Clean

let BuildNuSpecXml (name: string) (deps: seq<string * string>) =
    let e n = X.Element.Create n
    let ( -- ) (a: X.Element) (b: string) = X.Element.WithText b a
    e "package" - [
        e "metadata" - [
            e "id" -- name
            e "version" -- Config.Version
            e "authors"-- Config.Company
            e "owners"-- Config.Company
            e "licenseUrl" -- Config.LicenseUrl
            e "projectUrl"-- Config.Website
            e "requireLicenseAcceptance" -- "false"
            e "description" -- Config.Description
            e "copyright" -- sprintf "Copyright (c) %O %s" DateTime.Now.Year Config.Company
            e "tags" -- String.concat " " Config.Tags
            e "dependencies" - [
                e "group" - [
                    for (k, v) in deps ->
                        e "dependency" + ["id", k; "version", v]
                ]
            ]
        ]
        e "files" - [
            for fw in Frameworks do
                for ext in ["dll"; "xml"; "exe"] do
                    let pat = sprintf @"..\%s\bin\Release-%s\%s.*%s" name (fw.GetMSBuildLiteral()) name ext
                    let tgt = sprintf @"lib\%s" (fw.GetNuGetLiteral())
                    yield e "file" + ["src", pat; "target", tgt]
        ]
    ]

let BuildNuGet =
    T "BuildNuGet" <| fun () ->
        ensureDirectory DotBuildDir
        let nuSpec name = DotBuildDir +/ sprintf "%s.nuspec" name
        let pkgs =
            [
                "IntelliFactory.TypeScript", []
                "IntelliFactory.TypeScript.TypeProvider",
                    [
                        "IntelliFactory.TypeScript", Config.Version
                    ]
            ]
        let nuGetExe = RootDir +/ ".nuget" +/ "NuGet.exe"
        for (n, d) in pkgs do
            let spec = nuSpec n
            X.WriteFile (nuSpec n) (BuildNuSpecXml n d)
            spec
            |> NuGetPack (fun p ->
                { p with
                    OutputPath = DotBuildDir
                    ToolPath = nuGetExe
                    Version = Config.Version
                    WorkingDir = DotBuildDir
                })

BuildMain ==> BuildNuGet ==> Build

RunTargetOrDefault Build
