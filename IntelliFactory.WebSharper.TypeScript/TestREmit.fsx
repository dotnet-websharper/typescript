open System
open System.Reflection
open System.Reflection.Emit

let attrs =
    TypeAttributes.Class
    ||| TypeAttributes.Public
    ||| TypeAttributes.Sealed

let run () =
    let name = AssemblyName("MyAssem")
    let dom = AppDomain.CurrentDomain
    let asm = dom.DefineDynamicAssembly(name, AssemblyBuilderAccess.RunAndSave)
    let mB = asm.DefineDynamicModule("MyAssem.exe", "MyAssem.exe")
    let  t = mB.DefineType("A.B.C", TypeAttributes.Public ||| TypeAttributes.Class)
    t.CreateType() |> ignore
    System.IO.Path.GetFullPath("MyAssem.exe")
    |> printfn "WRITING %s"
    asm.Save("MyAssem.exe")

#r @"C:\Users\Anton\AppData\Local\Temp\MyAssem.exe"
let ty = A.B.C()



