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

/// Pickler combinators.
module internal Pickler =

    /// Implements serialization for a given type.
    [<Sealed>]
    type Pickler<'T>

    /// Reader abstraction for picklers.
    [<Sealed>]
    type PickleReader =

        /// Reads a value.
        member Read : Pickler<'T> -> 'T

    /// Writer abstraction for picklers.
    [<Sealed>]
    type PickleWriter =

        /// Writes a value.
        member Write : Pickler<'T> * 'T -> unit

    /// Alias for Pickler<'T>
    type T<'T> = Pickler<'T>

    /// Most general constructor for picklers.
    val FromPrimitives : (PickleReader -> 'T) -> (PickleWriter -> 'T -> unit) -> T<'T>

    /// Pickler for int.
    val Int : T<int>

    /// Pickler for int64.
    val Int64 : T<int64>

    /// Pickler for strings.
    val String : T<string>

    /// Pickler for timestamps.
    val DateTime : T<DateTime>

    /// Pickler for timestamps.
    val Wrap : ('T1 -> 'T2) -> ('T2 -> 'T1) -> T<'T1> -> T<'T2>

    /// Read a value from a byte array.
    val Unpickle : T<'T> -> byte [] -> 'T

    /// Write a value to a byte array.
    val Pickle : T<'T> -> 'T -> byte []

    /// Read a value from a stream.
    val ReadFromStream : T<'T> -> Stream -> 'T

    /// Write a value to a stream.
    val WriteToStream : T<'T> -> Stream -> 'T -> unit

    /// Pickler for list.
    val List : T<'T> -> T<list<'T>>

    /// Pickler for a sequence.
    val Seq : T<'T> -> T<seq<'T>>

    /// Pickler for options.
    val Option : T<'T> -> T<option<'T>>

    /// Pickler for `Choice<'A,'B>`.
    val Choice2 : T<'T1> -> T<'T2> -> T<Choice<'T1,'T2>>

    /// Pickler for a 2-tuple.
    val Pair : T<'T1> -> T<'T2> -> T<'T1 * 'T2>

    /// Pickler for a sequence of 2-tuples.
    val PairSeq : T<'T1> -> T<'T2> -> T<seq<'T1 * 'T2>>

    /// Pickler for a dictionary.
    val Dictionary<'T1,'T2 when 'T1 : equality> : T<'T1> -> T<'T2> -> T<Dictionary<'T1,'T2>>

    /// Unit pickler.
    val Unit : T<unit>

    /// Pickler for bytes.
    val Bytes : T<byte[]>

    /// Pickler for `System.Type` using the assembly-qualified name.
    val Type : T<Type>

    module ProductInternals =

        [<Sealed>]
        type Part<'R,'X,'Z>

    module PI = ProductInternals

    val DefProduct : evidence:'a -> mk:PI.Part<'b,'a,'c> -> Pickler<'b>
    val Field : proj:('a -> 'b) -> pickler:Pickler<'b> -> (PI.Part<'a,'c,'d> -> PI.Part<'a,('b -> 'c),('b * 'd)>)
    val EndProduct : unit -> PI.Part<'a,'a,unit>

    module SumInternals =

        [<Sealed>]
        type Part<'U,'T,'X,'Y>

    module SI = SumInternals

    val DefSum : evidence:('a -> 'b) -> part:SI.Part<'a,'b,'c,'c> -> Pickler<'a>
    val Case : inj:('a -> 'b) -> pickler:Pickler<'a> -> (SI.Part<'b,'c,'d,'e> -> SI.Part<'b,(('a -> 'e) -> 'c),Choice<'a,'d>,'e>)
    val LastCase : inj:('a -> 'b) -> pickler:Pickler<'a> -> SI.Part<'b,(('a -> 'c) -> 'c),'a,'c>
    val Variant : v:'a -> (SI.Part<'a,'b,'c,'d> -> SI.Part<'a,((unit -> 'd) -> 'b),Choice<unit,'c>,'d>)
    val LastVariant : v:'a -> SI.Part<'a,((unit -> 'b) -> 'b),unit,'b>
