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

namespace IntelliFactory.WebSharper.TypeScript.Tests

open System
open FParsec
open Fuchu
open IntelliFactory.WebSharper.TypeScript
module P = Parser
module S = Syntax

module internal ParserTests =

    let private parse s =
        let b = Names.NameBuilder.Create()
        let u = P.UserState.Create(b)
        match runParserOnString P.DeclarationSourceFile u "<text>" s with
        | Success (dsf, _, _) -> dsf
        | Failure (reason, _, _) -> failwith reason

    let private T1 =
        test "Parser1" {
            let (S.DSF res) = parse "declare class A { constructor(); }"
            match res with
            | [S.DE5 (_, S.AD3  cl)] ->
                cl.ClassName.Text =? "A"
                match cl.ClassBody with
                | [ S.ClassConstructor par ] -> ()
                | _ -> failwith "not a constructor"
            | _ ->
                failwith "not a class"
        }

    let AllTests =
        testList "ParserTests" [
            T1
        ]
