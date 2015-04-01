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

[<AutoOpen>]
module internal PathUtility =

    let PathDirectories =
        match Environment.GetEnvironmentVariable("PATH") with
        | "" | null -> []
        | p ->
            p.Split([| Path.PathSeparator |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList

    let LocateExecutableIn dirs name =
        dirs
        |> List.collect (fun dir ->
            let p1 = Path.Combine(dir, name)
            let p2 = Path.Combine(dir, name + ".exe")
            [p1; p2])
        |> List.filter File.Exists
        |> List.tryPick Some

    let LocateExecutable name =
        LocateExecutableIn PathDirectories name

    let FsiPath =
        match LocateExecutable "fsi" with
        | None ->
            let pfx86 = Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86)
            let dirs =
                [
                    Path.Combine(pfx86, "Microsoft SDKs/F#/3.0/Framework/v4.0")
                ]
            LocateExecutableIn dirs "fsi"
        | fsi -> fsi

    let GetFsiPath () =
        match FsiPath with
        | None -> failwith "Could not locate F# interactive (fsi)"
        | Some fsi -> fsi

    let MonoPath =
        let isRunningOnMono = Type.GetType("Mono.Runtime") <> null
        if isRunningOnMono
            then LocateExecutable "mono"
            else None
