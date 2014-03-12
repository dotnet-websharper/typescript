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
[<NoComparison>]
[<ReferenceEquality>]
type CompilerOptions =
    {

        /// The simple name of the assembly to generate, such as My.Assembly.
        AssemblyName : string

        /// Assembly version, if any.
        AssemblyVersion : option<Version>

        /// Resources to embed.
        EmbeddedResources : seq<EmbeddedResource>

        /// Reference assemblies.
        References : seq<ReferenceAssembly>

        /// Renaming to apply (defaults to None).
        Renaming : Renaming

        /// Root class or namespace where code is generated.
        Root : Root

        /// Path to an .snk file for signing, if any.
        StrongNameKeyFile : option<string>

        /// Folder to use for temporary storage.
        TemporaryFolder : string

        /// List of `.d.ts` files to process.
        TypeScriptDeclarationFiles : seq<FilePath>

        /// Verbosity level.
        Verbosity : Logging.Level

        /// WebSharper resource declarations to apply.
        WebSharperResources : seq<WebSharperResource>
    }

    /// Creates default options for a given assembly/namespace name and typings.
    static member Create(name, typings) =
        {
            AssemblyName = name
            AssemblyVersion = None
            EmbeddedResources = Seq.empty
            References = Seq.empty
            Renaming = Renaming.None
            Root = Root.Namespace name
            StrongNameKeyFile = None
            TemporaryFolder = Path.GetTempPath()
            TypeScriptDeclarationFiles = typings
            Verbosity = Logging.Warn
            WebSharperResources = Seq.empty
        }

    /// Utility to add signing to the `AssemblyName`.
    member internal this.BuildAssemblyName() =
        let n = AssemblyName(this.AssemblyName)
//        match this.StrongNameKeyFile with
//        | None -> ()
//        | Some file ->
//            let snk =
//                use fs = File.OpenRead(file)
//                new StrongNameKeyPair(fs)
//            n.KeyPair <- snk
        match this.AssemblyVersion with
        | Some v -> n.Version <- v
        | None -> ()
        n

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal CompilerOptions =

    let inline private ( ^ ) f x =
        f x

    let Pickler =
        Pickler.DefProduct (fun x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB ->
            {
                AssemblyName = x1
                AssemblyVersion = x2
                EmbeddedResources = x3
                References = x4
                Renaming = x5
                Root = x6
                StrongNameKeyFile = x7
                TemporaryFolder = x8
                TypeScriptDeclarationFiles = x9
                Verbosity = xA
                WebSharperResources = xB
            })
        ^ Pickler.Field (fun p -> p.AssemblyName) Pickler.String
        ^ Pickler.Field (fun p -> p.AssemblyVersion) (Pickler.Option Pickler.Version)
        ^ Pickler.Field (fun p -> p.EmbeddedResources) (Pickler.Seq EmbeddedResource.Pickler)
        ^ Pickler.Field (fun p -> p.References) (Pickler.Seq ReferenceAssembly.Pickler)
        ^ Pickler.Field (fun p -> p.Renaming) Renaming.Pickler
        ^ Pickler.Field (fun p -> p.Root) Root.Pickler
        ^ Pickler.Field (fun p -> p.StrongNameKeyFile) (Pickler.Option Pickler.String)
        ^ Pickler.Field (fun p -> p.TemporaryFolder) Pickler.String
        ^ Pickler.Field (fun p -> p.TypeScriptDeclarationFiles) (Pickler.Seq Pickler.String)
        ^ Pickler.Field (fun p -> p.Verbosity) Logging.Level.Pickler
        ^ Pickler.Field (fun p -> p.WebSharperResources) (Pickler.Seq WebSharperResource.Pickler)
        ^ Pickler.EndProduct ()
