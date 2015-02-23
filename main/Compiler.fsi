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

namespace WebSharper.TypeScript

/// Provides the main functionality: cross-compiling TypeScript
/// definitions for use in WebSharper.
module Compiler =
    type EmbeddedResource = global.WebSharper.TypeScript.EmbeddedResource
    type Level = global.WebSharper.TypeScript.Logging.Level
    type Message = global.WebSharper.TypeScript.Logging.Message
    type Options = global.WebSharper.TypeScript.CompilerOptions
    type ReferenceAssembly = global.WebSharper.TypeScript.ReferenceAssembly
    type Renaming = global.WebSharper.TypeScript.Renaming
    type Root = global.WebSharper.TypeScript.Root
    type WebSharperResource = global.WebSharper.TypeScript.WebSharperResource

    /// Represents a compiled assembly.
    [<Sealed>]
    type CompiledAssembly =

        /// The name of the assembly.
        member AssemblyName : string

        /// Returns the contents as raw bytes.
        member GetBytes : unit -> byte []

        /// The class or namespace name under which all generated code is nested.
        member Root : Root

    /// Represents the compilation result.
    [<Sealed>]
    type Result =

        /// The resulting assembly, if successulf.
        member CompiledAssembly : option<CompiledAssembly>

        /// Messages from the compiler.
        member Messages : seq<Message>

    /// Compiles a given configuration.
    val Compile : Options -> Result


