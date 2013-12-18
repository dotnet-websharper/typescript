open System
open System.Reflection
open System.Reflection.Emit

let attrs =
    TypeAttributes.Class
    ||| TypeAttributes.Public
    ||| TypeAttributes.Sealed

let NotImplementedConstructor =
    typeof<NotImplementedException>.GetConstructor(Array.empty)

let NotImplemented (m: MethodBuilder) =
    let gen = m.GetILGenerator()
    gen.Emit(OpCodes.Newobj, NotImplementedConstructor)
    gen.Emit(OpCodes.Throw)
    gen.Emit(OpCodes.Ret)

let run () =
    let name = AssemblyName("MyAssem")
    let dom = AppDomain.CurrentDomain
    let asm = dom.DefineDynamicAssembly(name, AssemblyBuilderAccess.RunAndSave)
    let mB = asm.DefineDynamicModule("MyAssem.exe", "MyAssem.exe")
    let t = mB.DefineType("A.B.C", TypeAttributes.Public ||| TypeAttributes.Interface)
    let defMethod () =
        let mD = t.DefineMethod("Identity", MethodAttributes.Static ||| MethodAttributes.Public)
        let gen = mD.DefineGenericParameters("T")
        mD.SetParameters([| gen.[0] :> Type |])
        mD.SetReturnType(gen.[0] :> Type)
        let par = mD.DefineParameter(1, ParameterAttributes.None, "x")
        NotImplemented mD
    defMethod ()
    defMethod ()
    t.CreateType() |> ignore
    System.IO.Path.GetFullPath("MyAssem.exe")
    |> printfn "WRITING %s"
    asm.Save("MyAssem.exe")

#r "../build/net45/Test.dll"




(*
    okay, so define "numeric" types


*)

