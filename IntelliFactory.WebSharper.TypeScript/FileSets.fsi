namespace IntelliFactory.WebSharper.TypeScript

/// Implements fileset construction. TypeScript programs consist of
/// a set of files (here we deal with `.d.ts` declaration source files only).
/// This module allows to construct such a set for an entry-point file by
/// traversing cross-references.
module FileSets =
    module Names = IntelliFactory.WebSharper.TypeScript.Names

    /// Configuration options.
    type Config =
        {
            /// `NameBuilder` used for parsing.
            NameBuilder : Names.NameBuilder

            /// Resolves an absolute module name to full file path.
            ResolveModule : string -> string
        }

    /// Represents a source file.
    type SourceFile =
        {
            /// References (`import`) to unknown external modules.
            ExternReferences : Set<string>

            /// Full path to the file.
            FullPath : string

            /// Set of absolute module names defined in-line in this source file
            /// (see `AmibentExternalModuleDeclaration` grammar production).
            KnownExternModules : Set<string>

            /// Full paths referenced with `<reference path=".." />` or else
            /// with relative `import` directives.
            ReferencePaths : Set<string>
        }

    /// Represents a set of source files considered together.
    type FileSet =
        {
            SourceFiles : list<SourceFile>
        }

    /// Computes and loads the file set from a single file.
    val LoadFileSet : Config -> fullPath: string -> FileSet
