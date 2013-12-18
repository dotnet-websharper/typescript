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

open Fuchu
module U = Uniplate

module internal UniplateTests =

    type Exp =
        | Add of Exp * Exp
        | Sub of Exp * Exp
        | Val of int

    let EW = U.Witness.Auto<Exp>()

    let E0 =
        Add (Add (Val 1, Val 4), Sub (Val 5, Val 6))

    let ExpTests =
        testList "Exp" [

            test "Children" {
                U.Children EW E0 =? [Add (Val 1, Val 4); Sub (Val 5, Val 6)]
            }

            test "Transform" {
                E0
                |> U.Transform EW (function
                    | Val n -> Val (n + 1)
                    | r -> r)
                    =? Add (Add (Val 2, Val 5), Sub (Val 6, Val 7))
            }

        ]

    type INode =
        abstract Match : U.R<INode>

    let NU =
        U.NodeUtility<INode>(fun x -> x.Match)

    type Collection<'T when 'T :> INode> =
        {
            Values : list<'T>
        }

        interface INode with
            member x.Match = NU.MatchList(x.Values, fun vs -> { Values = vs } :> INode)

    type B =
        | B of int

        interface INode with
            member x.Match = NU.Match x

    type A =
        | ANode of B
        | ATree of Collection<A>

        interface INode with
                member x.Match = NU.Match x

    type C =
        | CA of A
        | CB of B

        interface INode with
            member x.Match = NU.Match x

    type D =
        { Cs : Collection<C> }

        interface INode with
            member x.Match = NU.Match x

    let Col xs = { Values = xs }

    let D0 =
        {
            Cs =
                Col [
                    CA (ANode (B 1))
                    CB (B 2)
                    CA (ATree (Col [ANode (B 3); ANode (B 4)]))
                ]
        }

    let TryDowncast<'T when 'T :> INode>(x: INode) =
        match x with
        | :? 'T as r -> Some r
        | _ -> None

    let Select<'T when 'T :> INode> xs =
        Seq.choose TryDowncast<'T> xs

    let NodeTests =
        testList "Node" [
            test "Universe" {
                NU.Universe D0
                |> Select<B>
                |> Seq.toList =? [B 1; B 2; B 3; B 4]
            }
        ]

    let AllTests =
        testList "Uniplate" [
            ExpTests
            NodeTests
        ]
