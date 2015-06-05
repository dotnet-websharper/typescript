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

namespace WebSharper.TypeScript.Tests

open WebSharper
open WebSharper.Html.Client
open WebSharper.Testing

[<JavaScript>]
module ClientTests =

    let Main () =

        Test "Test0001" {
            let x = Tests.Test0001.x
            x.a <- 12.
            equal x.a 12.
            equal x.b "a"
            equal (box x.c) (box 0)
            equal x.d [||]
            isTrue x.e
            equal (x.incr(1., "A")) 2.
            equal (x.withRest(1., true, "A", "B")) "A,B,1,true"
        }

        Test "Test0002" {
            let w = Tests.Pervasives.window
            isTrue (w.outerHeight > 0.)
            isTrue (w.location.href.Contains("://"))
            isFalse (Tests.Pervasives.isNaN(1.))
            isTrue (Tests.Pervasives.isNaN(nan))
        }
