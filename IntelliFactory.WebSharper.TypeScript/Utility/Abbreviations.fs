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

open System
open System.Collections
open System.Text

[<AutoOpen>]
module internal Abbreviations =

    type Array = System.Array
    type Char = System.Char
    type BindingFlags = Reflection.BindingFlags
    type ConcurrentDictionary<'T1,'T2> = Concurrent.ConcurrentDictionary<'T1,'T2>
    type Dictionary<'T1,'T2> = Generic.Dictionary<'T1,'T2>
    type DirectoryInfo = IO.DirectoryInfo
    type Encoding = Text.Encoding
    type EqualityComparer<'T> = Generic.EqualityComparer<'T>
    type FileInfo = IO.FileInfo
    type Func<'T1,'T2> = System.Func<'T1,'T2>
    type HashSet<'T> = Generic.HashSet<'T>
    type Int32 = System.Int32
    type Interlocked = Threading.Interlocked
    type IComparable = System.IComparable
    type IDictionary<'T1,'T2> = Generic.IDictionary<'T1,'T2>
    type IEqualityComparer<'T> = Generic.IEqualityComparer<'T>
    type IReadOnlyDictionary<'T1,'T2> = Generic.IReadOnlyDictionary<'T1,'T2>
    type KeyValuePair<'T1,'T2> = Generic.KeyValuePair<'T1,'T2>
    type Object = System.Object
    type Path = IO.Path
    type PropertyInfo = Reflection.PropertyInfo
    type Regex = RegularExpressions.Regex
    type SortedSet<'T> = Generic.SortedSet<'T>
    type StringWriter = IO.StringWriter
    type TextWriter = IO.TextWriter
    type Type = System.Type
    type UnicodeCategory = Globalization.UnicodeCategory
    type UTF8Encoding = Text.UTF8Encoding

    /// Represents a system file path.
    type FilePath = string

    /// Default encoding is UTF-8.
    let private DefaultEncoding =
        let emitUTF8 = false
        let throwOnInvalid = true
        UTF8Encoding(emitUTF8, throwOnInvalid)

    /// Extensions for the Encoding type.
    type Text.Encoding with

        /// The default encoding, UTF-8.
        static member Default = DefaultEncoding

    /// Extensions for System.Char.
    type System.Char with

        /// Tabulates a function for efficient access on ASCII chars.
        static member Tabulate(f) =
            let t = [| for c in char 0 .. char 255 -> f c |]
            fun c ->
                let i = int c
                if i < 256 then t.[i] else f c

    /// List utilities.
    module List =

        let rec take n list =
            match n, list with
            | 0, _ -> []
            | n, h :: t -> h :: take (n - 1) t
            | _ -> []

        let inits list =
            [
                for i in 0 .. List.length list ->
                    take i list
            ]

