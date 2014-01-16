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

namespace IntelliFactory.WebSharper.TypeScript.Tests

open System
open Fuchu
open IntelliFactory.WebSharper.TypeScript
module C = GraphColoring

module internal GraphColoringTests =

    let private PickSmallestUniqueRef input =
        let h = Set.ofArray input
        let rec loop n = if h.Contains(n) |> not then n else loop (n + 1)
        loop 0

    let private PickSmallestUniqueRefTest =
        test "PickSmallestUnique" {
            let r = Random()
            let ri () = r.Next(5)
            let ra () =
                Seq.init (ri ()) (fun _ -> ri ())
                |> Seq.distinct
                |> Seq.sort
                |> Seq.toArray
            for _ in 1 .. 1000 do
                let a = ra ()
                C.PickSmallestUnique a =? PickSmallestUniqueRef a
        }

    let AllTests =
        testList "GraphColoring" [
            PickSmallestUniqueRefTest
        ]
