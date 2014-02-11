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

module E = ExternalModuleNames
module S = Syntax

/// Implements source file dependency resolution
/// according to section 11.1.1 of TypeScript manual 0.9.5.
module internal SourceFileDependencies =

    /// Resolves external module names to paths.
    [<Sealed>]
    type Resolver =

        /// Computes a module name from the file path.
        member ComputeName : FilePath -> option<E.TopLevelName>

        /// Finds the absolute system path
        /// for a given external module name.
        member Resolve : E.TopLevelName -> option<FilePath>

        /// Combines several resolers.
        static member Combine : seq<Resolver> -> Resolver

        /// Creates a new resolver.
        static member Create :
            (E.TopLevelName -> option<FilePath>)
            * (FilePath -> option<E.TopLevelName>) ->
            Resolver

        /// Resolves modules under a given system folder.
        static member InFolder : FilePath -> Resolver

        /// Resolves modules by trying given folders in sequence.
        static member InFolders : seq<FilePath> -> Resolver

        /// A resolver that always fail.s
        static member Failure : Resolver

    /// Configures the dependency resolution process.
    [<NoComparison>]
    [<NoEquality>]
    type Config =
        {
            /// The logger to report errors to.
            Logger : Logger

            /// The name builder.
            NameBuilder : Names.NameBuilder

            /// The resolver to use for external modules.
            Resolver : Resolver

            /// Source files to start searching from.
            StartFiles : seq<FilePath>
        }

    /// Represents a discovered source file.
    [<Sealed>]
    type SourceFile =

        /// The absolute path of the file in the file system.
        member FilePath : FilePath

        /// The associated module name if the file represents an external module.
        member ModuleName : option<E.TopLevelName>

        /// The parsed syntax of the source file.
        member Syntax : S.DeclarationSourceFile

    /// Represents the file resolution result.
    [<Sealed>]
    type Result =

        /// The set of resolved declaration source files.
        member SourceFiles : seq<SourceFile>

    /// Configures the resolution with given start files.
    val Configure : Names.NameBuilder -> Logger -> seq<FilePath> -> Config

    /// Performs source file dependency resolution.
    val Resolve : Config -> Result
