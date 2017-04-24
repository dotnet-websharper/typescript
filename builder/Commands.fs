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

#nowarn "40"

open System
open System.Diagnostics
open System.IO

module internal Commands =

    let SolutionDirectory =
        Path.Combine(__SOURCE_DIRECTORY__, "..")
        |> Path.GetFullPath

    let LocalPath p =
        p
        |> Printf.ksprintf (fun p ->
            Path.Combine(SolutionDirectory, p))

    let LocateExecutable name =
        let p0 = Path.Combine(SolutionDirectory, name)
        if File.Exists(p0) then Some p0 else
            PathUtility.LocateExecutable name

    let Exec file args =
        match LocateExecutable file with
        | None ->
            stderr.WriteLine("Not an executable command: {0}", file)
            1
        | Some file ->
            let info = ProcessStartInfo()
            info.WorkingDirectory <- SolutionDirectory
            info.WindowStyle <- ProcessWindowStyle.Hidden
            info.UseShellExecute <- false
            info.RedirectStandardInput <- true
            info.RedirectStandardError <- true
            info.RedirectStandardOutput <- true
            info.CreateNoWindow <- true
            info.FileName <- file
            info.Arguments <- args
            use p = new Process()
            p.StartInfo <- info
            p.EnableRaisingEvents <- true
            p.ErrorDataReceived.Add(fun data ->
                if String.IsNullOrEmpty(data.Data) |> not then
                    stderr.WriteLine(data.Data))
            p.OutputDataReceived.Add(fun data ->
                if String.IsNullOrEmpty(data.Data) |> not then
                    stdout.WriteLine(data.Data))
            p.Start() |> ignore
            p.StandardInput.Close()
            p.BeginErrorReadLine()
            p.BeginOutputReadLine()
            p.WaitForExit()
            p.ExitCode

    type Command =
        {
            Execute : unit -> bool
        }

        static member Combine a b =
            let exec () =
                if a.Execute () then
                    b.Execute ()
                else false
            { Execute = exec }

        member cmd.Run() =
            if cmd.Execute() then () else
                exit 1

    type CommandBuilder =
        | Command

        member x.Bind(a, b) =
            let exec () =
                if a.Execute () then
                    let b = b ()
                    b.Execute ()
                else false
            { Execute = exec }

        member x.Combine(a, b) =
            Command.Combine a b

        member x.Delay(f) =
            let exec () =
                let cmd = f ()
                cmd.Execute ()
            { Execute = exec }

        member x.For(xs, f) =
            xs
            |> Seq.map f
            |> Seq.fold Command.Combine (x.Return())

        member x.Return(()) =
            let exec () = true
            { Execute = exec }

        member x.Zero() =
            x.Return(())

    let Execute file fmt =
        fmt
        |> Printf.ksprintf (fun args ->
            let exec () = Exec file args = 0
            { Execute = exec })

    let Env x d =
        match Environment.GetEnvironmentVariable(x) with
        | null | "" -> d
        | v -> v

    let EnvOpt x =
        match Environment.GetEnvironmentVariable(x) with
        | null | "" -> None
        | v -> Some v

    let IsDir path =
        Path.Combine(SolutionDirectory, path)
        |> Directory.Exists

    let MakeDir path =
        let exec () =
            let d = Path.Combine(SolutionDirectory, path)
            if not (Directory.Exists d) then
                Directory.CreateDirectory(d) |> ignore
            true
        { Execute = exec }

    let FsiExec script args =
        Command {
            let fsiDir = Path.Combine(SolutionDirectory, "tools")
            let fsi = Path.Combine(fsiDir, "fsi.exe")
            let dir = Path.GetDirectoryName(PathUtility.GetFsiPath())
            let copy x =
                let o = Path.Combine(fsiDir, x)
                if not (File.Exists(o)) then
                    File.Copy(Path.Combine(dir, x), o)
            copy "Fsi.exe"
            copy "FSharp.Compiler.Interactive.Settings.dll"
            do! Execute fsi "--exec %s %s" script args
        }

    let CopyFileToDir file dir =
        Command {
            let file = LocalPath "%s" file
            let dir = LocalPath "%s" dir
            do! MakeDir dir
            do File.Copy(file, Path.Combine(dir, Path.GetFileName(file)), true)
        }
