// $begin{copyright}
//
// This file is confidential and proprietary.
//
// Copyright (c) IntelliFactory, 2004-2014.
//
// All rights reserved.  Reproduction or use in whole or in part is
// prohibited without the written consent of the copyright holder.
//-----------------------------------------------------------------
// $end{copyright}

namespace WebSharper.TypeScript

open System
open System.IO
open System.IO.Compression
open System.Net

module C = Commands

module internal Main =

    let PreReleaseDeps =
        [
        ]

    let ReleaseDeps =
        [
            "FParsec", "1.0.1"
            "Fuchu", "0.3.0.1"
            "WebSharper", "4.7.0.423"
            "WebSharper.FSharp", "4.7.0.423"
            "WebSharper.Testing", "4.7.0.423"
            "WebSharper.Html", "4.7.0.189"
        ]

    let NuGet args =
        C.Execute "tools/NuGet/NuGet.exe" args

    let InstallPre pkg =
        NuGet "install %s -nocache -pre -framework net461 -excludeVersion -o packages" pkg

    let InstallRel (name, ver) =
        NuGet "install %s -version %s -framework net461 -excludeVersion -o packages" name ver

    let Version =
        let f = Path.Combine(C.SolutionDirectory, "build", "version.txt")
        if File.Exists f then
            File.ReadAllText f
        else "4.0.0"

    let LocalNupkgPath p =
        C.LocalPath "build/%s.%s.nupkg" p Version

    let NuGetPackageOutputPath =
        C.Env "NuGetPackageOutputPath" (C.LocalPath "build")

    let NuGetPublishUrl =
        C.EnvOpt "NuGetPublishUrl"

    let NuGetApiKey =
        C.Env "NuGetApiKey" ""

    //let DownloadContrib =
    //    C.Command {
    //        if C.IsDir "contrib" |> not then
    //            let url = "http://github.com/borisyankov/DefinitelyTyped/archive/1.0.1.zip"
    //            do! C.MakeDir "contrib"
    //            do
    //                let file = C.LocalPath "contrib/DefinitelyTyped.zip"
    //                ServicePointManager.SecurityProtocol <- ServicePointManager.SecurityProtocol ||| SecurityProtocolType.Tls12
    //                use c = new WebClient()
    //                c.DownloadFile(url, file)
    //                ZipFile.ExtractToDirectory(file, C.LocalPath "contrib")
    //    }

    let Configure =
        C.Command {
            for pkg in PreReleaseDeps do
                do! InstallPre pkg
            for pkg in ReleaseDeps do
                do! InstallRel pkg
            //do! DownloadContrib
            do! C.FsiExec "scripts/computeVersion.fsx" ""
        }

    let BuildLib =
        C.Command {
            do! C.FsiExec "scripts/buildLib.fsx" ""
        }

    let NuGetPackages =
        [
            "WebSharper.TypeScript"
            "WebSharper.TypeScript.Lib"
        ]

    let MakeVsix =
        let make () =
            VisualStudioIntegration.BuildVsixFile {
                NuPkgs =
                    [
                        for p in NuGetPackages ->
                            {
                                Id = p
                                Path = LocalNupkgPath p
                            }
                    ]
                RootPath = C.SolutionDirectory
                VsixPath = C.LocalPath "build/WebSharper.TypeScript.%s.vsix" Version
            }
        C.Command {
            do make ()
        }

    let Deploy =
        let deploy f =
            C.CopyFileToDir f NuGetPackageOutputPath
        C.Command {
            if NuGetPackageOutputPath <> C.LocalPath "build" then
                for p in NuGetPackages do
                    do! deploy (LocalNupkgPath p)
                do! deploy (C.LocalPath "build/WebSharper.TypeScript.%s.vsix" Version)
        }

    let Publish =
        C.Command {
            match NuGetPublishUrl with
            | None -> ()
            | Some url ->
                for p in NuGetPackages do
                    do! NuGet """push "%s" "%s" -Source %s""" (LocalNupkgPath p) NuGetApiKey url
        }

    let Pack =
        C.Command {
            do! BuildLib
            do! C.MakeDir "build"
            for p in NuGetPackages do
                do! NuGet "pack %s.nuspec -outputDirectory build -Version %s" p Version
            do! MakeVsix
            do! Deploy
            do! Publish
        }

    let PrepareTests =
        C.Command {
            do! C.FsiExec "scripts/prepareTests.fsx" ""
        }

    let Test =
        C.Command {
            do! C.FsiExec "scripts/runTests.fsx" ""
        }

    [<EntryPoint>]
    let Start args =
        match List.ofArray args with
        | ["buildLib"] -> BuildLib.Run()
        | ["configure"] -> Configure.Run()
        | ["pack"] -> Pack.Run()
        | ["prepareTests"] -> PrepareTests.Run()
        | ["test"] -> Test.Run()
        | _ -> 
            eprintfn "unknown command line argument, use: buildLib/configure/pack/prepareTests/test"
            1
