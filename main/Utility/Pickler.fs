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

module Pickler =

    type PickleWriter =
        {
            BinaryWriter : BinaryWriter
        }

    type PickleReader =
        {
            BinaryReader : BinaryReader
        }

    type Pickler<'T> =
        {
            Pickle : PickleWriter -> 'T -> unit
            Unpickle : PickleReader -> 'T
        }

    type PickleWriter with
        member w.Write(p, x) =
            p.Pickle w x

    type PickleReader with
        member r.Read(p) =
            p.Unpickle r

    type T<'T> = Pickler<'T>

    let FromPrimitives read write =
        {
            Pickle = write
            Unpickle = read
        }

    let Int =
        FromPrimitives
            (fun r -> r.BinaryReader.ReadInt32())
            (fun w x -> w.BinaryWriter.Write(x))

    let Int64 =
        FromPrimitives
            (fun r -> r.BinaryReader.ReadInt64())
            (fun w x -> w.BinaryWriter.Write(x))

    let String =
        FromPrimitives
            (fun r ->
                let notNull = r.BinaryReader.ReadBoolean()
                if notNull then
                    r.BinaryReader.ReadString()
                else null)
            (fun w x ->
                match x with
                | null ->
                    w.BinaryWriter.Write(false)
                | s ->
                    w.BinaryWriter.Write(true)
                    w.BinaryWriter.Write(s))

    let Wrap f1 f2 p =
        FromPrimitives
            (fun r -> f1 (p.Unpickle r))
            (fun w x -> p.Pickle w (f2 x))

    let DateTime =
        Int64
        |> Wrap DateTime.FromBinary
            (fun d -> d.ToBinary())

    let ReadFromStream p s =
        use r = new BinaryReader(s, encoding = Encoding.Default)
        p.Unpickle { BinaryReader = r }

    let WriteToStream p s v =
        use w = new BinaryWriter(s, encoding = Encoding.Default)
        p.Pickle { BinaryWriter = w } v

    let Unpickle p bytes =
        use s = new MemoryStream(bytes: byte[])
        ReadFromStream p s

    let Pickle p v =
        use s = new MemoryStream()
        WriteToStream p s v
        s.ToArray()

    let Unit =
        FromPrimitives
            (fun r -> ())
            (fun w () -> ())

    let Bytes =
        FromPrimitives
            (fun r ->
                let n = r.BinaryReader.ReadInt32()
                r.BinaryReader.ReadBytes(n))
            (fun w x ->
                w.BinaryWriter.Write(Array.length x)
                w.BinaryWriter.Write(x))

    let Seq p =
        FromPrimitives
            (fun r ->
                let out = ResizeArray()
                let rec loop () =
                    let n = r.BinaryReader.ReadBoolean()
                    if n then
                        let v = r.Read(p)
                        out.Add(v)
                        loop ()
                loop ()
                out.ToArray() :> seq<_>)
            (fun w xs ->
                for x in xs do
                    w.BinaryWriter.Write(true)
                    w.Write(p, x)
                w.BinaryWriter.Write(false))

    let List p =
        Wrap List.ofSeq List.toSeq (Seq p)

    let Pair p1 p2 =
        FromPrimitives
            (fun r ->
                let a = r.Read(p1)
                let b = r.Read(p2)
                (a, b))
            (fun w (x, y) ->
                w.Write(p1, x)
                w.Write(p2, y))

    let PairSeq p1 p2 =
        Seq (Pair p1 p2)

    let Dictionary p1 p2 =
        PairSeq p1 p2
        |> Wrap
            (fun xs ->
                let d = Dictionary()
                for (k, v) in xs do
                    d.[k] <- v
                d)
            (fun d ->
                seq {
                    for KeyValue (k, v) in d do
                        yield (k, v)
                })

    let Choice2 p1 p2 =
        FromPrimitives
            (fun r ->
                let first = r.BinaryReader.ReadBoolean()
                if first then
                    let a = r.Read(p1)
                    Choice1Of2 a
                else
                    let b = r.Read(p2)
                    Choice2Of2 b)
            (fun w x ->
                match x with
                | Choice1Of2 x ->
                    w.BinaryWriter.Write(true)
                    w.Write(p1, x)
                | Choice2Of2 x ->
                    w.BinaryWriter.Write(false)
                    w.Write(p2, x))

    let Type =
        String
        |> Wrap
            (fun s -> Type.GetType(s, throwOnError = true))
            (fun t -> t.AssemblyQualifiedName)

    module ProductInternals =

        type Part<'R,'X,'Z> =
            | P of ('R -> 'Z) * ('X -> 'Z -> 'R) * Pickler<'Z>

        let pp f g t =
            P (f, g, t)

        let finish () =
            pp ignore (fun r () -> r) Unit

        let defProduct e p =
            match p with
            | P (f, g, t) ->
                Wrap (g e) f t

        let defField proj tf p =
            match p with
            | P (g, h, tr) ->
                pp
                    (fun rr -> (proj rr, g rr))
                    (fun c fx -> h (c (fst fx)) (snd fx))
                    (Pair tf tr)

    module PI = ProductInternals

    let DefProduct evidence mk =
        PI.defProduct evidence mk

    let Field proj pickler =
        PI.defField proj pickler

    let EndProduct () =
        PI.finish ()

    module SumInternals =

        type Part<'U,'T,'X,'Y> =
            | P of Pickler<'X> * ('X -> 'U) * (('X -> 'Y) -> ('T -> 'Y))

        let defP p f g =
            P (p, f, g)

        let defLastCase inj p =
            defP p inj (fun h t -> t h)

        let defNextCase inj p (P (tr, xu, f)) =
            defP (Choice2 p tr)
                (function
                    | Choice1Of2 x -> inj x
                    | Choice2Of2 x -> xu x)
                (fun g h ->
                    f (fun x -> g (Choice2Of2 x))
                        (h (fun x -> g (Choice1Of2 x))))

        let defSum ev (P (tr, xu, f)) =
            Wrap xu (fun u -> f (fun x -> x) (ev u)) tr

    module SI = SumInternals

    let DefSum evidence part =
        SI.defSum evidence part

    let Case inj pickler =
        SI.defNextCase inj pickler

    let LastCase inj pickler =
        SI.defLastCase inj pickler

    let Variant v =
        Case (fun () -> v) Unit

    let LastVariant v =
        LastCase (fun () -> v) Unit

    let Option p =
        let inline ( ^ ) f x = f x
        DefSum (fun x k1 k2 ->
            match x with
            | None -> k1 ()
            | Some r -> k2 r)
        ^ Variant None
        ^ LastCase Some p

    let Fix f =
        let rec read r = res.Unpickle r
        and write w x = res.Pickle w x
        and p = FromPrimitives read write
        and res = f p
        res
