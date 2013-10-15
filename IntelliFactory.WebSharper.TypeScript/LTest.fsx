#r "../packages/FParsec.1.0.1/lib/net40-Client/FParsecCS.dll"
#r "../packages/FParsec.1.0.1/lib/net40-Client/FParsec.dll"
#load "Names.fs"
#load "Lexer.fs"
#load "Syntax.fs"
#load "Parser.fs"

open FParsec
open System.IO
module Lexer = IntelliFactory.WebSharper.TypeScript.Lexer
module P = IntelliFactory.WebSharper.TypeScript.Parser
module S = IntelliFactory.WebSharper.TypeScript.Syntax
module Names = IntelliFactory.WebSharper.TypeScript.Names

let p = @"C:\Users\Anton\Documents\GitHub\DefinitelyTyped\jquery\jquery.d.ts"

let mk () : Lexer.UserState =
    { ReferencePaths = []; CurrentPath = "."; IdBuilder = Names.NameBuilder.Create() }

let testStr (p: IntelliFactory.WebSharper.TypeScript.Parser.P<'T>) (i: string) =
    runParserOnString p (mk ()) i

let testFile (f: FileInfo) =
    let result =
        System.Text.UTF8Encoding(false, true)
        |> runParserOnFile P.DeclarationSourceFile (mk ()) p
    match result with
    | Success (res, st, posi) ->
        printfn "%s -> OK" f.Name
    | Failure (str, err, _) ->
        printfn "%s ERROR: %s %O" f.Name str err

let testAll () =
    let defT = @"C:\Users\Anton\Documents\GitHub\DefinitelyTyped"
    for f in Directory.EnumerateFiles(defT, "*.d.ts", SearchOption.AllDirectories) do
        testFile (FileInfo f)
