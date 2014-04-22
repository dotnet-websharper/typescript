/// Uses the IntelliFactory.WebSharper.TypeScript API to compile
/// `lib.d.ts` to `IntelliFactory.WebSharper.TypeScript.Lib.dll`.

#r "../build/Release/IntelliFactory.WebSharper.TypeScript.dll"
#r "../build/Release/Mono.Cecil.dll"

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

open Mono.Cecil

let importType (asmd: AssemblyDefinition) (t: System.Type) =
    let asm =
        t.Assembly.Location
        |> AssemblyDefinition.ReadAssembly
    let ty =
        asm.MainModule.GetType(t.FullName)
        |> asmd.MainModule.Import
    ty.Resolve()

let fixup (path: string) =
    let asm = AssemblyDefinition.ReadAssembly(path)
    let stringType = importType asm typeof<string>
    let targetFA = importType asm typeof<System.Runtime.Versioning.TargetFrameworkAttribute>
    let ctor =
        targetFA.Methods
        |> Seq.find (fun m -> m.IsConstructor && not m.IsStatic && m.Parameters.Count = 1)
        |> asm.MainModule.Import
    let attr = CustomAttribute(ctor)
    attr.ConstructorArguments.Add(CustomAttributeArgument(stringType, ".NETFramework,Version=v4.0"))
    let namedArg = CustomAttributeNamedArgument("FrameworkDisplayName", CustomAttributeArgument(stringType, ".NET Framework 4"))
    attr.Properties.Add(namedArg)
    asm.CustomAttributes.Add(attr)
    asm.Write(path)

let main () =
    let result =
        {
            C.Options.Create(name, [p ["Lib.d.ts"]]) with
                AssemblyName = name
                AssemblyVersion = Some (Version "2.5.0.0")
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
        fixup path
        stdout.WriteLine("Written: {0}", path)

do main ()


