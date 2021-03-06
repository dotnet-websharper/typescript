﻿// $begin{copyright}
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

module C = Contracts
module D = SourceFileDependencies

/// Traverses parsed syntax to discover modules and contracts,
/// construct scope structure, resolve names and spit out the
/// defined contracts and values.
module internal Analysis =

    /// Represents a statically accessible value.
    [<Sealed>]
    type Value =

        /// Suggested access path on the CLR.
        member HintPath : NamePath

        /// The original access path in TypeScript.
        member NamePath : NamePath

        /// The type of the value.
        member Type : C.Type

    /// Inputs for the analysis.
    type Input =
        {
            Logger : Logger
            MetadataTable : Metadata.Table
            NameBuilder : Names.NameBuilder
            SourceFiles : seq<D.SourceFile>
        }

    /// Discovered contracts and values.
    type Output =
        {
            Contracts : seq<C.Contract>
            Values : seq<Value>
        }

    /// Performs the analysis path.
    val Analyze : Input -> Output
