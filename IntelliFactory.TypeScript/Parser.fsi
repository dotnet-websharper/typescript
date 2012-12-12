/// Implements parsing of TypeScript 0.8 definitions (.d.ts) files.
module internal IntelliFactory.TypeScript.Declarations.Parser

open System.IO
open IntelliFactory.Parsec
open IntelliFactory.TypeScript
module S = Syntax

val Parse : log: Log -> loc: Resolver.Location -> ParseResult<S.DeclarationSourceFile,unit,int>

