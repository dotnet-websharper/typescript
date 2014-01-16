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

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Testing

[<JavaScript>]
module ClientTests =

    let Main () =

        Test "Test0001" {
            let x = Tests.Test0001.x
            x.a <- 12
            x.a =? 12
            x.b =? "a"
            box x.c =? box 0
            x.d =? [||]
            x.e =? true
            x.incr(1, "A") =? 2
            x.withRest(1, true, "A", "B") =? "A,B,1,true"
        }

        Test "Test0002" {
            let w = Tests.window
            (w.outerHeight > 0) =? true
            w.location.href.Contains("://") =? true
            Tests.isNaN(1) =? false
            Tests.isNaN(As nan) =? true
        }
