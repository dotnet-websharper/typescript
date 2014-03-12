/// Uses the IntelliFactory.WebSharper.TypeScript API to compile
/// `lib.d.ts` to `IntelliFactory.WebSharper.TypeScript.Lib.dll`.

#load "../build/buildLib.includes.fsx"
#r "../build/net40/IntelliFactory.WebSharper.TypeScript.dll"

open System
open System.IO
module C = IntelliFactory.WebSharper.TypeScript.Compiler

let p xs =
    Path.Combine [|
        yield __SOURCE_DIRECTORY__
        yield! xs
    |]

let name =
    "IntelliFactory.WebSharper.TypeScript.Lib"

let snk =
    Path.Combine [|
        Environment.GetEnvironmentVariable("INTELLIFACTORY")
        "keys"
        "IntelliFactory.snk"
    |]

let main () =
    let result =
        {
            C.Options.Create(name, [p ["Lib.d.ts"]]) with
                AssemblyName = name
                AssemblyVersion = Some (Version "2.5.0.0")
                StrongNameKeyFile = Some snk
                References =
                    [
                        C.ReferenceAssembly.File @"C:\Program Files (x86)\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll"
                    ]
        }
        |> C.Compile

    for msg in result.Messages do
        stdout.WriteLine(msg)

    match result.CompiledAssembly with
    | None -> failwith "Failed to compile Lib.d.ts"
    | Some assem ->
        let bytes = assem.GetBytes()
        let path = p [".."; "build"; "net40"; name + ".dll"]
        File.WriteAllBytes(path, bytes)
        stdout.WriteLine("Written: {0}", path)

do main ()
