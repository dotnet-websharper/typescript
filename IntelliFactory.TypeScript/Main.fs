module IntelliFactory.TypeScript.Main

open System
open System.Diagnostics
open System.IO
open IntelliFactory.Parsec
open IntelliFactory.TypeScript.Declarations
module DP = Parser
module DL = Lexer
module DS = Syntax

[<EntryPoint>]
let main args =
    let example = args.[0]
    match DP.Parse Log.Default (Resolver.Location.Create example) with
    | Parsed (x, _, _, _) ->
        let ns =
            IntelliFactory.TypeScript.Declarations.Builder.Build
                ["IntelliFactory"; "WebSharper"] "Node" x
        do
            use w = new StringWriter()
            CSharp.Compile w ns
            let f = Path.ChangeExtension(example, ".cs")
            File.WriteAllText(f, w.ToString())
    | ParseFailed e ->
        printfn "FAIL: %s --> %O" example e
    0


//type C<'T> = P.Parser<'T,char,unit>
//
//let lexeme : C<int> =
//    (1 <=> P.String "1;") <|> (0 <=> P.String "0;")
//
//let grammar : P.Parser<seq<int>,int,unit> =
//    P.Many (P.AnyToken ()) <*< P.End ()
//
//let test (s: string) =
//    let tS = P.ParseSequence lexeme () (P.TokenStream.FromString "?" s)
//    let cfg : P.TokenStream.SequenceConfig<_> =
//        {
//            Positioning = P.TokenStream.Absolute (fun t -> P.SourcePosition("?", 0, 0))
//            ChunkSize = 2
//            SourceName = "<?>"
//        }
//    match P.Parse grammar () (P.TokenStream.FromTokenSequence cfg tS) with
//    | P.Parsed ok ->
//        printfn "PARSED"
//        for p in ok do
//            printfn " ==> %O" p
//    | P.Failed e ->
//        printfn "ERROR: %O" e
//
//test "1;F"
