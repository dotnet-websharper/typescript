// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

namespace WebSharper.TypeScript

module Logging =

    type Level =
        | Verbose
        | Info
        | Warn
        | Error
        | Critical

        override this.ToString() =
            match this with
            | Verbose -> "Verbose"
            | Info -> "Info"
            | Warn -> "Warn"
            | Error -> "Error"
            | Critical -> "Critical"

    type Message =
        {
            L : Level
            T : string
        }

        override m.ToString() =
            String.Format("{0}: {1}", m.L, m.T)

        member m.Level = m.L
        member m.Text = m.T

        static member Create(l, t) =
            { L = l; T = t }

    [<Sealed>]
    type Logger(level: Level) =
        let msgs = HashSet()

        let send lev m =
            Message.Create(lev, m)
            |> msgs.Add |> ignore

        member l.Log(lev, fmt) =
            if lev >= level then
                send lev fmt

        member l.Log(lev, fmt, v: obj) =
            if lev >= level then
                String.Format(fmt, v)
                |> send lev

        member l.Log(lev, fmt, [<ParamArray>] objs: obj []) =
            if lev >= level then
                String.Format(fmt, objs)
                |> send lev

        member l.Exception(e: exn) =
            l.Log(Critical, string e)

        member l.FailedToResolveTypeName(name: string) =
            l.Log(Warn,  "Failed to resolve type name: {0}", name)

        member l.MissingFile(fP: FilePath) =
            l.Log(Error,  "MissingFile: {0}", fP)

        member l.NoParse(fP: FilePath, reason: string) =
            l.Log(Error,  "NoParse [{0}]: {1}", fP, reason)

        member l.UnknownFileType(fP: FilePath) =
            l.Log(Error, "UnknownFileType: {0}", fP)

        member l.UnresolvedExternalModule(n: string) =
            l.Log(Error, "UnresolvedExternalModule: {0}", n)

        member l.All =
            msgs :> seq<_>

    let inline ( ^ ) f x = f x

    let LoggingLevelPickler =
        Pickler.DefSum (fun x k1 k2 k3 k4 k5 ->
            match x with
            | Verbose -> k1 ()
            | Info -> k2 ()
            | Warn -> k3 ()
            | Error -> k4 ()
            | Critical -> k5 ())
        ^ Pickler.Variant Verbose
        ^ Pickler.Variant Info
        ^ Pickler.Variant Warn
        ^ Pickler.Variant Error
        ^ Pickler.LastVariant Critical

    type Level with
        static member Pickler = LoggingLevelPickler

    let MessagePickler =
        Pickler.DefProduct (fun l t -> Message.Create(l, t))
        ^ Pickler.Field (fun (m: Message) -> m.Level) LoggingLevelPickler
        ^ Pickler.Field (fun (m: Message) -> m.Text) Pickler.String
        ^ Pickler.EndProduct()

    type Message with
        static member Pickler = MessagePickler

type internal Logger = Logging.Logger
