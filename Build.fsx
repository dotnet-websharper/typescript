#load "tools/includes.fsx"

open IntelliFactory.Build

let bt = BuildTool().PackageId("WebSharper.TypeScript", "2.5-alpha")

let main =
    bt.FSharp.Library("IntelliFactory.WebSharper.TypeScript")
        .Modules(
            [
                "Memoization"
                "NewLexer"
            ])
        .References(fun r ->
            [
                r.NuGet("FParsec").Reference()
            ])

bt.Solution [
    main
]
|> bt.Dispatch

//#if BOOT
//open System
//open System.Net
//open System.Security
//open Fake
//module FB = Fake.Boot
//
//let ReadPassword () =
//    let p = new SecureString()
//    Console.Write("Password: ")
//    let rec loop () =
//        let key = Console.ReadKey(true)
//        if key.Key = ConsoleKey.Enter then
//            Console.WriteLine()
//        else
//            p.AppendChar(key.KeyChar)
//            Console.Write("*")
//            loop ()
//    loop ()
//    p
//
//let ReadCredential () =
//    let user =
//        Console.Write("User: ")
//        Console.ReadLine()
//    let pw = ReadPassword ()
//    NetworkCredential(user, pw)
//
//let Credential =
//    lazy ReadCredential()
//
//let Credentials =
//    {
//        new ICredentials with
//            member this.GetCredential(uri: Uri, authType) =
//                Console.WriteLine("Authorizing to {0}: ", uri)
//                Credential.Value
//    }
//
//FB.Prepare {
//    FB.Config.Default __SOURCE_DIRECTORY__ with
//        NuGetSourceUrl = "https://www.myget.org/F/intellifactory/"
//        NuGetCredentials = Some Credentials
//        NuGetDependencies =
//            let ( ! ) x = FB.NuGetDependency.Create x
//            [
//                !"IntelliFactory.Build"
//                !"IntelliFactory.Parsec"
//                !"WebSharper"
//            ]
//}
//#else
//
//#load ".build/boot.fsx"
//open System
//open System.IO
//open Fake
//module B = IntelliFactory.Build.CommonBuildSetup
//module F = IntelliFactory.Build.FileSystem
//module NG = IntelliFactory.Build.NuGetUtils
//
//let cfg = Fake.Boot.Config.Default "A"
//
//let RootDir = __SOURCE_DIRECTORY__
//let ( +/ ) a b = Path.Combine(a, b)
//let T x f = Target x f; x
//
//module Config =
//    let Company = "IntelliFactory"
//    let PackageId = "IntelliFactory.TypeScript"
//    let VersionSuffix = "alpha"
//
//    let Description = "WebSharper-compatible TypeScript analyzer and bindings compiler"
//    let LicenseUrl = "http://websharper.com/licensing"
//    let Tags = ["WebSharper"; "TypeScript"; "F#"]
//    let NuGetVersion = global.NuGet.SemanticVersion("0.0.4-alpha")
//    let AssemblyVersion = Version "0.0.0.0"
//    let Website = "http://bitbucket.org/IntelliFactory/typescript"
//
//    let FileVersion =
//        let v = NuGetVersion.Version
//        let bn = environVarOrNone "BUILD_NUMBER"
//        match bn with
//        | None -> v
//        | Some bn -> new Version(v.Major, v.Minor, v.Build, int bn)
//
//let Metadata =
//    let m = B.Metadata.Create()
//    m.Author <- Some Config.Company
//    m.AssemblyVersion <- Some Config.AssemblyVersion
//    m.FileVersion <- Some Config.FileVersion
//    m.Description <- Some Config.Description
//    m.Product <- Some Config.PackageId
//    m.Website <- Some Config.Website
//    m
//
//let Net40 : B.BuildConfiguration =
//    {
//        ConfigurationName = "Release"
//        Debug = false
//        FrameworkVersion = B.Net40
//        NuGetDependencies =
//            new global.NuGet.PackageDependencySet(B.Net40.ToFrameworkName(),
//                [new global.NuGet.PackageDependency("IntelliFactory.Parsec")])
//    }
//
//let Configs = [Net40]
//
//let DefProject name : B.Project =
//    {
//        BuildConfigurations = Configs
//        MSBuildProjectFilePath = Some (RootDir +/ name +/ (name + ".fsproj"))
//        Name = name
//    }
//
//let Projects =
//    [
//        DefProject "IntelliFactory.TypeScript"
//        DefProject "IntelliFactory.TypeScript.TypeProvider"
//    ]
//
//let Solution =
//    B.Solution(RootDir, Metadata = Metadata, Projects = Projects)
//
//let BuildMain = T "BuildMain" (fun () -> Solution.MSBuild() |> Async.RunSynchronously)
//let Build = T "Build" ignore
//
//let Clean =
//    T "Clean" (fun () ->
//        Solution.MSBuild
//            {
//                Targets = ["Clean"]
//                BuildConfiguration = None
//                Properties = Map.empty
//            }
//        |> Async.RunSynchronously)
//
//let ComputePublishedFiles (c: B.BuildConfiguration) =
//    let config = "Release-" + c.FrameworkVersion.GetMSBuildLiteral()
//    let prefix = RootDir +/ "*" +/ "bin" +/ config
//    (!+ (prefix +/ "*.dll")
//        ++ (prefix +/ "*.xml")
//        ++ (prefix +/ "*.exe")
//        ++ (prefix +/ "*.exe.config"))
//    |> Scan
//    |> Seq.distinctBy Path.GetFileName
//
//let NuGetPackageFile =
//    RootDir +/ ".build" +/ sprintf "%s.%O.nupkg" Config.PackageId Config.NuGetVersion
//
//let BuildNuGet = T "BuildNuGet" <| fun () ->
//    let content =
//        use out = new MemoryStream()
//        let builder = new NuGet.PackageBuilder()
//        builder.Id <- Config.PackageId
//        builder.Version <- Config.NuGetVersion
//        builder.Authors.Add(Config.Company) |> ignore
//        builder.Owners.Add(Config.Company) |> ignore
//        builder.LicenseUrl <- Uri(Config.LicenseUrl)
//        builder.ProjectUrl <- Uri(Config.Website)
//        builder.Copyright <- String.Format("Copyright (c) {0} {1}", DateTime.Now.Year, Config.Company)
//        builder.Description <- Config.Description
//        Config.Tags
//        |> Seq.iter (builder.Tags.Add >> ignore)
//        for c in Configs do
//            ComputePublishedFiles c
//            |> Seq.map (fun file ->
//                let ppf = global.NuGet.PhysicalPackageFile()
//                ppf.SourcePath <- file
//                ppf.TargetPath <- "tools" +/ c.FrameworkVersion.GetNuGetLiteral() +/ Path.GetFileName(file)
//                ppf)
//            |> Seq.distinctBy (fun file -> file.TargetPath)
//            |> Seq.iter builder.Files.Add
//        builder.Save(out)
//        F.Binary.FromBytes (out.ToArray())
//        |> F.BinaryContent
//    content.WriteFile(NuGetPackageFile)
//    tracefn "Written %s" NuGetPackageFile
//
//BuildMain ==> BuildNuGet ==> Build
//
//RunTargetOrDefault Build
//
//#endif
