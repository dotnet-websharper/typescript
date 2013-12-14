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

module Naming =

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

    type IGraphNode =
        abstract Color : int with get, set
        abstract Edges : seq<IGraphNode>

    type IGraphColorer =
        abstract Colorize : seq<IGraphNode> -> unit

    [<Sealed>]
    type Id private (stem: string) =
        let edges = HashSet<Id>()
        let mutable color = 0

        member n.Link(o: Id) =
            if n.Stem = stem then
                edges.Add(o) |> ignore
                o.Link(n)

        override n.ToString() =
            match color with
            | 0 -> stem
            | k -> stem + string color

        member val Color = 0 with get, set
        member n.Stem = stem
        member n.Text = n.ToString()

        static member Create(name) =
            Id(MakeValidIdentifier name)

        static member LinkAll(names: seq<Id>) =
            let names = Seq.toArray names
            for i in 0 .. names.Length - 1 do
                for j in i + 1 .. names.Length - 1 do
                    names.[i].Link(names.[j])

        interface IGraphNode with
            member n.Color with get () = color and set x = color <- x
            member n.Edges = Seq.cast edges

    [<Sealed>]
    type Module<'T>(id: 'T) =

        member val SubContracts = ResizeArray<KeyValuePair<Id,C.Contract>>()
        member val SubModules = ResizeArray<Module<Id>>()
        member val SubValues = ResizeArray<KeyValuePair<Id,A.Value>>()

        member m.Contracts : seq<Id * C.Contract> = Seq.empty
        member m.Modules : seq<Module<Id>> = Seq.empty
        member m.Id = id
        member m.Values : seq<Id * A.Value> = Seq.empty

    type NestedModule = Module<Id>
    type TopModule = Module<unit>

    let Memo f =
        M.Memoize M.Options.Default f

    let MemoRec f =
        M.MemoizeRecursive M.Options.Default f

    let BuildModule (out: A.Output) =
        let topContainer = TopModule()
        let getContainer =
            MemoRec <| fun getContainer path ->
                match path with
                | Names.NP1 name -> Module(Id.Create(name.Text))
                | Names.NP2 (p, name) ->
                    let p = getContainer p
                    let m = Module(Id.Create(name.Text))
                    p.SubModules.Add(m)
                    m
        for c in out.Contracts do
            let (contracts, name) =
                match c.HintPath with
                | Names.NP1 name -> (topContainer.SubContracts, name)
                | Names.NP2 (path, name) -> (getContainer.[path].SubContracts, name)
            contracts.Add(KeyValuePair(Id.Create(name.Text), c))
        for v in out.Values do
            let (values, name) =
                match v.NamePath with
                | Names.NP1 name -> (topContainer.SubValues, name)
                | Names.NP2 (path, name) -> (getContainer.[path].SubValues, name)
            values.Add(KeyValuePair(Id.Create(name.Text), v))
        topContainer

    let rec LinkNames<'T> (path: list<Id>) (m: Module<'T>) : unit =
        Id.LinkAll <| seq {
            yield! path
            for m in m.Modules do
                yield m.Id
            for (c, _) in m.Contracts do
                /// TODO: contracts - link member names.
                yield c
            for (v, _) in m.Values do
                yield v
        }
        for sM in m.Modules do
            LinkNames (sM.Id :: path) sM

    let Do out =
        let m = BuildModule out
        LinkNames [] m
        /// TODO: colorize names
        m
