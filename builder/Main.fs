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

namespace IntelliFactory.WebSharper.TypeScript

open System
open System.IO
open System.IO.Compression
open System.Net

module C = Commands

module internal Main =

    let PreReleaseDeps =
        [
            "WebSharper"
        ]

    let ReleaseDeps =
        [
            "FParsec", "1.0.1"
            "Fuchu", "0.3.0.1"
        ]

    let NuGet args =
        C.Execute "tools/NuGet/NuGet.exe" args

    let InstallPre pkg =
        NuGet "install %s -nocache -pre -excludeVersion -o packages" pkg

    let InstallRel (name, ver) =
        NuGet "install %s -version %s -excludeVersion -o packages" name ver

    let Version =
        sprintf "3.0.%s-alpha" (C.Env "BUILD_NUMBER" "0")

    let NuGetPackageOutputPath =
        C.Env "NuGetPackageOutputPath" (C.LocalPath "build")

    let DownloadContrib =
        C.Command {
            if C.IsDir "contrib" |> not then
                let url = "http://github.com/borisyankov/DefinitelyTyped/archive/master.zip"
                do! C.MakeDir "contrib"
                do
                    let file = C.LocalPath "contrib/DefinitelyTyped.zip"
                    use c = new WebClient()
                    c.DownloadFile(url, file)
                    ZipFile.ExtractToDirectory(file, C.LocalPath "contrib")
        }

    let Configure =
        C.Command {
            for pkg in PreReleaseDeps do
                do! InstallPre pkg
            for pkg in ReleaseDeps do
                do! InstallRel pkg
            do! DownloadContrib
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
                                Path = C.LocalPath "build/%s.%s.nupkg" p Version
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
                    do! deploy (C.LocalPath "build/%s.%s.nupkg" p Version)
                do! deploy (C.LocalPath "build/WebSharper.TypeScript.%s.vsix" Version)
        }

    let Pack =
        C.Command {
            do! BuildLib
            do! C.MakeDir "build"
            for p in NuGetPackages do
                do! NuGet "pack %s.nuspec -outputDirectory build -Version %s" p Version
            do! MakeVsix
            do! Deploy
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
        let res x = if x then 0 else 1
        match List.ofArray args with
        | ["buildLib"] -> BuildLib.Execute() |> res
        | ["configure"] -> Configure.Execute() |> res
        | ["pack"] -> Pack.Execute() |> res
        | ["prepareTests"] -> PrepareTests.Execute() |> res
        | ["test"] -> Test.Execute() |> res
        | _ -> 1
