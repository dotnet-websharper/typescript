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

/// Configuration options for the TypeScript compiler.
type CompilerOptions =
    {
        AssemblyName : string
        EmbeddedResources : seq<EmbeddedResource>
        References : seq<ReferenceAssembly>
        Renaming : Renaming
        Root : Root
        TemporaryFolder : string
        TypeScriptDeclarationFiles : seq<FilePath>
        Verbosity : Logging.Level
        WebSharperResources : seq<WebSharperResource>
    }

    /// Creates default options for a given assembly and namespace name.
    static member Create(name: string, typings) =
        {
            AssemblyName = name
            EmbeddedResources = Seq.empty
            References = Seq.empty
            Renaming = Renaming.None
            Root = Root.Namespace name
            TemporaryFolder = Path.GetTempPath()
            TypeScriptDeclarationFiles = typings
            Verbosity = Logging.Warn
            WebSharperResources = Seq.empty
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal CompilerOptions =

    let inline private ( ^ ) f x =
        f x

    let Pickler =
        Pickler.DefProduct (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 ->
            {
                AssemblyName = x1
                EmbeddedResources = x2
                References = x3
                Renaming = x4
                Root = x5
                TemporaryFolder = x6
                TypeScriptDeclarationFiles = x7
                Verbosity = x8
                WebSharperResources = x9
            })
        ^ Pickler.Field (fun p -> p.AssemblyName) Pickler.String
        ^ Pickler.Field (fun p -> p.EmbeddedResources) (Pickler.Seq EmbeddedResource.Pickler)
        ^ Pickler.Field (fun p -> p.References) (Pickler.Seq ReferenceAssembly.Pickler)
        ^ Pickler.Field (fun p -> p.Renaming) Renaming.Pickler
        ^ Pickler.Field (fun p -> p.Root) Root.Pickler
        ^ Pickler.Field (fun p -> p.TemporaryFolder) Pickler.String
        ^ Pickler.Field (fun p -> p.TypeScriptDeclarationFiles) (Pickler.Seq Pickler.String)
        ^ Pickler.Field (fun p -> p.Verbosity) Logging.Level.Pickler
        ^ Pickler.Field (fun p -> p.WebSharperResources) (Pickler.Seq WebSharperResource.Pickler)
        ^ Pickler.EndProduct ()
