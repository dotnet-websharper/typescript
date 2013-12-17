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

namespace IntelliFactory.WebSharper.TypeScript

module A = Analysis
module C = Contracts
module M = Memoization
module S = Shapes

module Ident =

    let ValidIdentifierRegex =
        Regex(@"[_a-zA-Z][_a-zA-Z0-9]*")

    let InvalidCharacterRegex =
        Regex(@"[^_a-zA-Z0-9]")

    let KeywordSet =
        @"
            Item New Call

            abstract as base bool break byte case
            catch char checked class const continue decimal
            default delegate do double else enum event
            explicit extern false finally fixed float for
            foreach goto if implicit in int interface
            internal is lock long namespace new null
            object operator out override params private protected
            public readonly ref return sbyte sealed short
            sizeof stackalloc static string struct switch this
            throw true try typeof uint ulong unchecked
            unsafe ushort using virtual void volatile while

            asr land lor lsl lsr lxor mod sig

            abstract and as assert base begin class default delegate do
            done downcast downto elif else end exception extern false
            finally for fun function global if in inherit inline interface
            internal lazy let let! match member module mutable namespace
            new not null of open or override private public rec return return!
            select static struct then to true try type upcast use use!
            void when while with yield yield!

            atomic break checked component const constraint constructor
            continue eager event external fixed functor
            include method mixin object parallel process protected pure
            sealed tailcall trait virtual volatile
        "
        |> fun s ->
            HashSet(Regex.Split(s, @"\s+"))

    let IsKeyword k =
        KeywordSet.Contains(k)
        || k.StartsWith("get_")
        || k.StartsWith("set_")

    let TrailingNumbersRegex =
        Regex("\d+$")

    let Replace (p: Regex) (replace: string) (orig: string) =
        p.Replace(orig, replace)

    let MakeValidIdentifier name =
        let r =
            name
            |> Replace InvalidCharacterRegex "_"
            |> Replace TrailingNumbersRegex ""
        if ValidIdentifierRegex.IsMatch(r) && not (IsKeyword name)
            then name
            else "_" + name

    [<NoComparison>]
    [<ReferenceEquality>]
    type Id =
        {
            mutable Color : int
            mutable Edges : HashSet<Id>
            Stem : string
        }

        override id.ToString() =
            match id.Color with
            | 0 -> id.Stem
            | k -> id.Stem + string k

        member id.Text =
            id.ToString()

    [<NoComparison>]
    [<ReferenceEquality>]
    type Builder =
        {
            All : ResizeArray<Id>
        }

        static member Create() =
            { All = ResizeArray() }

    let CreateId ids name =
        let r =
            {
                Color = 0
                Edges = HashSet()
                Stem = MakeValidIdentifier name
            }
        ids.All.Add(r)
        r

    let LinkTo a b =
        a.Edges.Add(b) |> ignore

    let Link a b =
        if a.Stem = b.Stem then
            LinkTo a b
            LinkTo b a

    let LinkAll names =
        let names = Seq.toArray names
        for i in 0 .. names.Length - 1 do
            for j in i + 1 .. names.Length - 1 do
                Link names.[i] names.[j]

    let Disambiguate ids =
        GraphColoring.ColorGraph {
            new GraphColoring.IConfig<Id> with
                member cfg.Edges(id) = id.Edges :> seq<_>
                member cfg.GetColor(id) = id.Color
                member cfg.SetColor(id, c) = id.Color <- c
                member cfg.Nodes = ids.All :> seq<_>
        }

    type Builder with
        member x.Disambiguate() = Disambiguate x
        member x.Id(name) = CreateId x name
        member x.LinkAll(ids) = LinkAll ids
