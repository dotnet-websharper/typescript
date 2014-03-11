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

namespace IntelliFactory.WebSharper.TypeScript

/// Provides the main functionality: cross-compiling TypeScript
/// definitions for use in WebSharper.
module TypeScriptCompiler =

    /// Represents a reference assembly.
    [<Sealed>]
    type ReferenceAssembly =

        /// An assembly file located at a given path.
        static member File : path: string -> ReferenceAssembly

        /// In-memory assembly represented by raw bytes.
        static member Raw : bytes: byte [] -> ReferenceAssembly

    /// Configures the TypeScript cross-compilation process.
    type Config =
        {
            /// The name of the generated assembly.
            AssemblyName : string

            /// Embedded resources.
            EmbeddedResources : seq<EmbeddedResource>

            /// References used by the compilation process.
            References : seq<ReferenceAssembly>

            /// Temporary folder to use.
            TemporaryFolder : string

            /// The class name under which all generated code is nested.
            TopLevelClassName : string

            /// TypeScript declaration files to process.
            TypeScriptDeclarationFiles : seq<FilePath>

            /// Verbosity of the logging output.
            Verbosity : Logging.Level

            /// WebSharper resources defined a the assembly level.
            WebSharperResources : seq<WebSharperResource>
        }

    /// Represents a compiled assembly.
    [<Sealed>]
    type CompiledAssembly =

        /// The name of the assembly.
        member AssemblyName : string

        /// Returns the contents as raw bytes.
        member GetBytes : unit -> byte []

        /// The class name under which all generated code is nested.
        member TopLevelClassName : string

    /// Represents the compilation result.
    [<Sealed>]
    type Result =

        /// The resulting assembly, if successulf.
        member CompiledAssembly : option<CompiledAssembly>

        /// Messages from the compiler.
        member Messages : seq<Logging.Message>

    /// Compiles a given configuration.
    val Compile : Config -> Result

    /// Configures the compilation process.
    val Configure : topLevelClassName: string -> paths: seq<FilePath> -> Config

