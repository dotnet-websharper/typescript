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

module N = Naming

open IntelliFactory.WebSharper

/// Implements assembly generation via `System.Reflection.Emit`.
module internal ReflectEmit =

    [<Sealed>]
    type EmbeddedResource =
        static member Create : name: string * content: byte[] -> EmbeddedResource

    /// Configures assembly generation.
    type Config =
        {
            AssemblyName : string
            EmbeddedResources : seq<EmbeddedResource>
            TemporaryFolder : string
            TopLevelClassName : string
            TopModule : N.TopModule
        }

    /// Generates an assembly.
    val ConstructAssembly : Config -> byte []
