// NOTE: executing this script is currently failing, though
// X.dll passes PEVerify. Problem with importing static generic methods
// in TSTP logic?

#r "../build/net45/IntelliFactory.WebSharper.TypeScript.dll"
#r "../build/net45/IntelliFactory.WebSharper.TypeScript.TypeProvider.dll"

open IntelliFactory.WebSharper.TypeScript

type G = TypeScriptGenerator<file="../defs/example1.d.ts"> //, output="X.dll">

let test () =
    G.mapArray((fun x -> x + 1), Array.empty)

// let () = G.f<int>(1);

//let that () =
//    G.mapArray (fun x -> x + 1) [| 1 .. 10 |]


//let that =
//    G.mapArray (fun x -> x + 1) [| 1 .. 20 |]

//open System
//open System.IO
//open System.Reflection
//open System.Reflection.Emit
//
//type tc =
//    {
//        AssemblyDirectory : string
//        AssemblyName : AssemblyName
//        ClassName : string
//        ParameterValues : obj []
//    }
//
//let MakeASM tc =
//    let assemblyName = tc.AssemblyName
//    let shortName = assemblyName.Name
//    let fileName = shortName + ".dll"
//    let dom = AppDomain.CurrentDomain
//    let aB = dom.DefineDynamicAssembly(name = assemblyName, access = AssemblyBuilderAccess.Save, dir = tc.AssemblyDirectory)
//    let mB = aB.DefineDynamicModule(name = shortName, fileName = fileName, emitSymbolInfo = false)
//    let tB = mB.DefineType(tc.ClassName, TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract)
//    let methB =
//        tB.DefineMethod(tc.ParameterValues.[0] :?> string,
//            System.Reflection.MethodAttributes.Public
//            ||| System.Reflection.MethodAttributes.Abstract
//            ||| System.Reflection.MethodAttributes.Virtual)
//    methB.SetReturnType(typeof<int>)
//    tB.CreateType() |> ignore
//    aB.Save(fileName)
//    printfn "Saved %s" fileName
//
//let Test () =
//    MakeASM {
//        AssemblyDirectory = @"C:\Users\Anton\home\dev\tmp"
//        AssemblyName = AssemblyName("ALEPH")
//        ClassName = "Aleph"
//        ParameterValues = [| box "Foo" |]
//    }
