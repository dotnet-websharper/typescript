#r "../build/net45/FParsecCS.dll"
#r "../build/net45/FParsec.dll"
#r "../build/net45/IntelliFactory.WebSharper.TypeScript.dll"
//#r "../build/net45/IntelliFactory.WebSharper.TypeScript.TypeProvider.dll"

open System
open System.IO
open IntelliFactory.WebSharper

module C = TypeScript.TypeScriptCompiler

let res =
    {
        C.Configure "Ex" [Path.Combine(__SOURCE_DIRECTORY__, "..", "defs", "example2.d.ts")] with
            Verbosity = TypeScript.Logging.Verbose
    }
    |> C.Compile

for msg in res.Messages do
    printfn "%O" msg

match res.CompiledAssembly with
| Some asm ->
    let p = Path.Combine(__SOURCE_DIRECTORY__, "..", "Example1.dll")
    File.WriteAllBytes(p, asm.GetBytes())
    printfn "Written: %s" p
| None ->
    printfn "FAILED"
