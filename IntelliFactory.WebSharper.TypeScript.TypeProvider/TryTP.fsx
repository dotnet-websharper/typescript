#r "../build/net45/FParsecCS.dll"
#r "../build/net45/FParsec.dll"
#r "../build/net45/IntelliFactory.WebSharper.TypeScript.dll"
//#r "../build/net45/IntelliFactory.WebSharper.TypeScript.TypeProvider.dll"

open System
open System.IO
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.TypeScript

module C = TypeScriptCompiler

let asm =
    C.Configure "A.B.C" [Path.Combine(__SOURCE_DIRECTORY__, "..", "typescript", "example1.d.ts")]
    |> C.Compile

File.WriteAllBytes(Path.Combine(__SOURCE_DIRECTORY__, "Example1.dll"), asm.GetBytes())
