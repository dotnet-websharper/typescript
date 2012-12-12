/// Builds WebSharper interface assemblies based on Declarations files.
module internal IntelliFactory.TypeScript.Declarations.Builder

open System
open System.IO
module S = Syntax
module C = IntelliFactory.TypeScript.CSharp

val Build : ns: seq<string> -> n: string -> file: S.DeclarationSourceFile -> C.Namespace
