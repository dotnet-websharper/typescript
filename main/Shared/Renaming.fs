// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

/// Specifies a renaming strategy applied to CLR names
/// generated during TypeScript compilation.
[<NoComparison>]
[<ReferenceEquality>]
type Renaming =
    private
    | RCustom of Type
    | RKeep
    | RPrefix of string

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Renaming =

    type IStrategy =
        abstract Rename : list<string> -> list<string>

    let internal Prepare (nB: Names.NameBuilder) (r: Renaming) : NamePath -> NamePath =
        let toList np =
            let rec c acc (np: NamePath) =
                match np with
                | NamePath.NP1 x -> x.Text :: acc
                | NamePath.NP2 (x, n) -> c (n.Text :: acc) x
            c [] np
        let (!) x = nB.CreateName(x)
        let fromList def (list: list<string>) =
            match list with
            | [] -> def
            | [x] -> NamePath.NP1 !x
            | x :: xs -> List.fold (fun s x -> NamePath.NP2 (s, !x)) (NamePath.NP1 !x) xs
        match r with
        | RCustom t ->
            let s = Activator.CreateInstance(t) :?> IStrategy
            fun n ->
                toList n
                |> s.Rename
                |> fromList n
        | RKeep -> fun x -> x
        | RPrefix p ->
            let rec r n =
                match toList n with
                | [] | [_] -> n
                | x :: xs when x = p -> fromList n xs
                | _ -> n
            r

    let inline private ( ^ ) f x =
        f x

    let internal Pickler =
        Pickler.DefSum (fun x k1 k2 k3 ->
            match x with
            | RCustom f -> k1 f
            | RKeep -> k2 ()
            | RPrefix p -> k3 p)
        ^ Pickler.Case RCustom Pickler.Type
        ^ Pickler.Variant RKeep
        ^ Pickler.LastCase RPrefix Pickler.String

type Renaming with

    /// Specify a custom renaming strategy.
    static member Custom<'T when 'T : (new : unit -> 'T)
                            and 'T :> Renaming.IStrategy>() =
        RCustom typeof<'T>

    /// Remove a given prefix.
    static member RemovePrefix(prefix: string) =
        RPrefix prefix

    /// Do not rename any names.
    static member None = RKeep
