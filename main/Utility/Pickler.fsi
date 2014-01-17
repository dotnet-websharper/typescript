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

    /// Experimental support for n-way product types such as records.
    /// See `product` and `field` combinators.
    module ProductInternals =

        /// Internal type for type-checking intermediate values.
        [<Sealed>]
        type Part<'R,'X,'Z> 

        /// Internal type for type-checking intermediate values.
        [<Sealed>]
        type Wrap<'T> =

            /// Defines an extra field.
            static member ( ^+ ) : Wrap<'A->'B> * 'A -> 'B

            /// Defines the last field.
            static member ( ^. ) : Wrap<'A->'B> * Wrap<Part<'C,'C,unit>->'A> -> 'B

    module PI = ProductInternals

    /// Starts defining a pickler for an n-ary product, such as
    /// record. Example:
    ///
    ///    type Person =
    ///        {
    ///            Address : string
    ///            Age : int
    ///            Name : string
    ///        }
    ///
    ///    let makePerson name age address =
    ///        {
    ///            Address = address
    ///            Age = age
    ///            Name = name
    ///        }
    ///
    ///    let personPickler =
    ///        Pickler.Product makePerson
    ///        ^+ Pickler.Field (fun p -> p.Name) Pickler.String
    ///        ^+ Pickler.Field (fun p -> p.Age) Pickler.Int
    ///        ^. Pickler.Field (fun p -> p.Address) Pickler.String
    ///
    /// The implementation is not currently efficient, though it
    /// may improve in the future.
    val Product : 'A -> PI.Wrap<PI.Part<'B,'A,'C> -> Pickler<'B>>

    /// See `product`.
    val Field : ('A -> 'B) -> Pickler<'B> -> PI.Wrap<PI.Part<'A,'C,'D> -> PI.Part<'A,'B->'C,'B*'D>>

    /// Experimental support for n-way sum types such as unions. See `Sum`.
    module SumInternals =

        /// Internal type for type-checking intermediate values.
        [<Sealed>]
        type Part<'U,'T,'X,'Y>

        /// Internal type for type-checking intermediate values.
        [<Sealed>]
        type Case<'T1,'T2> =

            /// Adds a case.
            static member ( ^+ ) :
                Case<'A->'B,Pickler<'A>> * Wrap<Part<'B,'C,'D,'E>> ->
                Wrap<Part<'B,('A->'E)->'C,Choice<'A,'D>,'E>>
 
            /// Adds the last case.
            static member ( ^. ) :
                Case<'A->'B,Pickler<'A>> * Case<'C->'B,Pickler<'C>> ->
                Wrap<Part<'B,('A->'D)->('C->'D)->'D,Choice<'A,'C>,'D>>

        /// Internal type for type-checking intermediate values.
        and [<Sealed>] Wrap<'T> =

            /// Adds a case.
            static member ( ^+ ) : Wrap<'A->'B> * Wrap<'A> -> 'B

            /// Adds the last case.
            static member ( ^. ) :
                Wrap<Part<'A,('B->'C)->'C,'B,'C>->'D> * Case<'B->'A,Pickler<'B>> -> 'D

    module SI = SumInternals

    /// Starts defining a pickler for an n-ary sum type, such as
    /// a union type. For example:
    ///
    ///    type UnionT =
    ///        | Case1
    ///        | Case2 of int
    ///        | Case3 of string * int
    ///
    ///    let unionTPickler =
    ///        Pickler.Sum (fun x k1 k2 k3 ->
    ///            match x with
    ///            | Case1 -> k1 ()
    ///            | Case2 x -> k2 x
    ///            | Case3 (x, y) -> k3 (x, y))
    ///        ^+ Pickler.Variant Case1
    ///        ^+ Pickler.Case Case2 Pickler.Int
    ///        ^. Pickler.Case Case3 (Pickler.Pair Pickler.String Pickler.Int)
    ///
    /// Note that the implementation is not currently efficient,
    /// though it may improve in the future.
    val Sum : ('A -> 'B) -> SI.Wrap<SI.Part<'A,'B,'C,'C> -> Pickler<'A>>

    /// See `sum`.
    val Case : 'A -> 'B -> SI.Case<'A,'B>

    /// Useful for union cases without arguments.
    val Variant : 'A -> SI.Case<unit->'A,Pickler<unit>>
