﻿// $begin{copyright}
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

module Memoization =

    type Options<'T1,'T2> =
        {
            Equality : IEqualityComparer<'T1>
            Laziness : option<Lazy<'T2> -> 'T2>
        }

    module Options =

        let Default<'T1,'T2> : Options<'T1,'T2> =
            {
                Equality = EqualityComparer<'T1>.Default :> IEqualityComparer<'T1>
                Laziness = None
            }

    let Configure () =
        Options.Default

    [<NoComparison>]
    [<NoEquality>]
    type Table<'T1,'T2> =
        {
            Dictionary : Dictionary<'T1,'T2>
            Lookup : 'T1 -> 'T2
            Options : Options<'T1,'T2>
            Root : obj
        }

        member this.Clear() =
            lock this.Root <| fun () ->
                this.Dictionary.Clear()

        member this.Get(key: 'T1) : 'T2 =
            this.Lookup key

        member this.GetKeys() : seq<'T1> =
            fun () ->
                let keys = this.Dictionary.Keys
                let a = Array.zeroCreate keys.Count
                keys.CopyTo(a, 0)
                a
            |> lock this.Root
            |> Array.toSeq

        member this.Remove(key: 'T1) : bool =
            lock this.Root <| fun () ->
                this.Dictionary.Remove(key)

        member this.Equality =
            this.Options.Equality

        member this.Count =
            lock this.Root <| fun () ->
                this.Dictionary.Count

        member this.Item
            with get (key: 'T1) : 'T2 =
                this.Lookup key

    let Memoize<'T1,'T2> (cfg: Options<'T1,'T2>) (f: 'T1 -> 'T2) : Table<'T1,'T2> =
        let cache = Dictionary(cfg.Equality)
        let root = obj ()
        let lookup =
            match cfg.Laziness with
            | Some pack ->
                fun x ->
                    let r =
                        lock root <| fun () ->
                            match cache.TryGetValue(x) with
                            | true, y -> Lazy.CreateFromValue(y)
                            | _ ->
                                let r = lazy f x
                                cache.[x] <- pack r
                                r
                    r.Value
            | None ->
                fun x ->
                    lock root <| fun () ->
                        match cache.TryGetValue(x) with
                        | true, y -> y
                        | _ ->
                            let r = f x
                            cache.[x] <- r
                            r
        {
            Dictionary = cache
            Lookup = lookup
            Options = cfg
            Root = root
        }

    let MemoizeRecursive<'T1,'T2> (cfg: Options<'T1,'T2>) (f: ('T1 -> 'T2) -> ('T1 -> 'T2)) : Table<'T1,'T2> =
        let rec g x = f h.Get x
        and h : Table<'T1,'T2> = Memoize cfg g
        h

    type Options<'T1,'T2> with
        member opts.Memoize(f) = Memoize opts f
        member opts.MemoizeRecursive(f) = MemoizeRecursive opts f

