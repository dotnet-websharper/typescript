/// Uses the IntelliFactory.WebSharper.TypeScript API to compile
/// `lib.d.ts` to `IntelliFactory.WebSharper.TypeScript.Lib.dll`.

#load "../build/buildLib.includes.fsx"
#r "../build/net45/IntelliFactory.WebSharper.TypeScript.dll"

open System
open System.IO
open IntelliFactory.WebSharper.TypeScript

module C = TypeScriptCompiler

let p xs =
    Path.Combine [|
        yield __SOURCE_DIRECTORY__
        yield! xs
    |]

let main () =
    let result =
        C.Configure "Tests" [p ["Lib.d.ts"]]
        |> C.Compile

    for msg in result.Messages do
        stdout.WriteLine(msg)

    match result.CompiledAssembly with
    | None -> failwith "Failed to compile Lib.d.ts"
    | Some assem ->
        let bytes = assem.GetBytes()
        let path = p [".."; "build"; "net45"; "IntelliFactory.WebSharper.TypeScript.Lib.dll"]
        File.WriteAllBytes(path, bytes)
        stdout.WriteLine("Written: {0}", path)

do main ()
