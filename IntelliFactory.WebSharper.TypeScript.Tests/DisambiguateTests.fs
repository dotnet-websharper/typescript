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
open Fuchu
module D = Disambiguate

module internal DisambiguateTests =

    type Name =
        | Name1 of string * int
        | Name2 of Name * string * int

    let rec WithAncestors n =
        match n with
        | D.Name1 _ -> [n]
        | D.Name2 (parent, _, _) -> n :: WithAncestors parent

    let rec RewriteName (s: D.Solution) name =
        match name with
        | D.Name1 (name, suffix) ->
            Name1 (name, s.[suffix])
        | D.Name2 (parent, name, suffix) ->
            Name2 (RewriteName s parent, name, s.[suffix])

    let ContainsNoDuplicates xs =
        let xs = Seq.cache xs
        Seq.length xs = Seq.length (Seq.distinct xs)

    let IsCorrect (problem: D.Problem) solution =
        problem.Names
        |> Seq.collect WithAncestors
        |> Seq.distinct
        |> Seq.map (RewriteName solution)
        |> ContainsNoDuplicates

    type Generator<'T> =
        {
            Next : Random -> 'T
        }

    module Generator =

        let test n g f =
            let r = Random()
            for _ in 1 .. n do
                let c = g.Next(r)
                f c =? true

        let define f =
            { Next = f }

        let oneOf (xs: 'T[]) =
            define <| fun r ->
                xs.[r.Next(xs.Length)]

        let listOf gen =
            define <| fun r ->
                let k = r.Next(5)
                [for i in 1 .. k -> gen.Next(r)]

        let pair g1 g2 =
            define <| fun r ->
                (g1.Next(r), g2.Next(r))

        let filter f g =
            let rec loop r =
                let c = g.Next(r)
                if f c then c else loop r
            define loop

        let map f g =
            define <| fun r ->
                f (g.Next(r))

    let var =
        [| for i in 1 .. 4 -> D.Var.Create() |]
        |> Generator.oneOf

    let text =
        Generator.oneOf [| "a"; "b" |]

    let name =
        Generator.listOf (Generator.pair text var)
        |> Generator.filter (fun xs -> not xs.IsEmpty)
        |> Generator.map (fun ((hn, hs) :: t) ->
            List.fold (fun s (h, n) -> D.Name2 (s, h, n))
                (D.Name1 (hn, hs))
                t)

    let problem =
        Generator.listOf name
        |> Generator.map (fun names ->
            let p : D.Problem = { Names = names }
            p)

    let AllTests =
        testList "Disambiguate" [
            test "DisambiguationTest" {
                Generator.test 10000 problem (fun p ->
                    IsCorrect p (D.Solve p))
            }
        ]
