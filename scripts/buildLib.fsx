/// Uses the WebSharper.TypeScript API to compile
/// `lib.d.ts` to `WebSharper.TypeScript.Lib.dll`.

#r "../build/Release/WebSharper.TypeScript.dll"
#r "../build/Release/Mono.Cecil.dll"

open System
open System.IO
module C = WebSharper.TypeScript.Compiler

let p xs =
    Path.Combine [|
        yield __SOURCE_DIRECTORY__
        yield! xs
    |]

let name =
    "WebSharper.TypeScript.Lib"

let snk =
    Path.Combine [|
        Environment.GetEnvironmentVariable("INTELLIFACTORY")
        "keys"
        "IntelliFactory.snk"
    |]

open Mono.Cecil

let importType (asmd: AssemblyDefinition) (t: System.Type) =
    let asm =
        t.Assembly.Location
        |> AssemblyDefinition.ReadAssembly
    let ty =
        asm.MainModule.GetType(t.FullName)
        |> asmd.MainModule.Import
    ty.Resolve()

let main () =
    let result =
        {
            C.Options.Create(name, [p ["Lib.d.ts"]]) with
                AssemblyName = name
                AssemblyVersion = Some (Version "3.0.0.0")
                StrongNameKeyFile = Some snk
                References =
                    [
                        C.ReferenceAssembly.File (p ["../build/Release/FSharp.Core.dll"])
                    ]
        }
        |> C.Compile

    for msg in result.Messages do
        stdout.WriteLine(msg)

    match result.CompiledAssembly with
    | None -> failwith "Failed to compile Lib.d.ts"
    | Some assem ->
        let bytes = assem.GetBytes()
        let path = p [".."; "build"; "Release"; name + ".dll"]
        File.WriteAllBytes(path, bytes)
        stdout.WriteLine("Written: {0}", path)

do main ()


