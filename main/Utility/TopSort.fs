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

namespace WebSharper.TypeScript

module TopSort =

    let Explicit (roots: seq<'T>) (pred: 'T -> seq<'T>) (nodeIdentity: IEqualityComparer<'T>) =
        seq {
            let visited = HashSet(nodeIdentity)
            let rec visit node =
                seq {
                    if visited.Add(node) then
                        for pN in pred node do
                            yield! visit pN
                        yield node
                }
            for node in roots do
                yield! visit node
        }

    let Intrinsic (roots: seq<'T>) (pred: 'T -> seq<'T>) =
        seq {
            let visited = HashSet()
            let rec visit node =
                seq {
                    if visited.Add(node) then
                        for pN in pred node do
                            yield! visit pN
                        yield node
                }
            for node in roots do
                yield! visit node
        }
