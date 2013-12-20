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

module internal Lexer =
    open FParsec

    type Id = Names.Name

    type UserState =
        {
            IdBuilder : Names.NameBuilder
            IsAfterNewline : bool
            ReferencePaths : Set<string>
        }

        static member Create : Names.NameBuilder -> UserState

    type L<'T> = Parser<'T,UserState>

    val Identifier : L<Id>
    val IdentifierName : L<Names.Name>
    val StringLiteral : L<string>
    val IntegerLiteral : L<int>

    val ``.`` : L<unit>
    val ``,`` : L<unit>
    val ``;`` : L<unit>
    val ``(`` : L<unit>
    val ``)`` : L<unit>
    val ``{`` : L<unit>
    val ``}`` : L<unit>
    val ``[`` : L<unit>
    val ``]`` : L<unit>
    val LessThan : L<unit>
    val GreaterThan : L<unit>
    val Extends : L<unit>
    val Any : L<unit>
    val Number : L<unit>
    val String : L<unit>
    val Void : L<unit>
    val TypeOf : L<unit>
    val Boolean : L<unit>
    val ``=>`` : L<unit>
    val New : L<unit>
    val Interface : L<unit>
    val Import : L<unit>
    val Export : L<unit>
    val Equal : L<unit>
    val Require : L<unit>
    val ``:`` : L<unit>
    val ``?`` : L<unit>
    val ``...`` : L<unit>
    val Class : L<unit>
    val Implements : L<unit>
    val Enum : L<unit>
    val Var : L<unit>
    val Function : L<unit>
    val Module : L<unit>
    val Constructor : L<unit>
    val Declare : L<unit>
    val Public : L<unit>
    val Private : L<unit>

    module Flags =
        val ``?`` : L<bool>
        val Export : L<bool>
        val Static : L<bool>

    val ActualOrImpliedSemicolon : L<unit>

    val Make : L<'T> -> L<'T>
