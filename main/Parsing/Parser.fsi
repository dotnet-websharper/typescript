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

open FParsec
module S = Syntax

/// Parses TypeScript 0.9.5 declaration files.
module internal Parser =

    /// State of the parser.
    type UserState = Lexer.UserState

    /// Type of the parser.
    type Parser<'T> = Parser<'T,UserState>

    /// Grammar for DeclarationSourceFile production.
    val DeclarationSourceFile : Parser<S.DeclarationSourceFile>

    /// Parse result.
    type Result =

        /// Parse failure with explanation.
        | ParseFailed of string

        /// Resutling AST and `<reference>` comment paths.
        | ParseOk of S.DeclarationSourceFile * seq<FilePath>

    /// Parses a given file.
    val ParseFile : Names.NameBuilder -> FilePath -> Result
