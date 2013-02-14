namespace IntelliFactory.TypeScript

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Threading
open System.Text
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Data.TypeProviders
open Microsoft.FSharp.Quotations

[<Sealed>]
[<TypeProvider>]
type TypeProvider(config: TypeProviderConfig) =

    static do Assembly.InstallLoadHack()
    static let logFile = @"C:\Users\toyvo\home\dev\tmp\TP.log"
    static let root = obj ()
    static let date = DateTimeOffset.UtcNow
    static let domainName = AppDomain.CurrentDomain.FriendlyName
    static let log =
        {
            new Logging.Log() with
                override this.Send(level, message) =
                    lock root <| fun () ->
                        let msg = String.Format("[DOMAIN={0} @ {1}] [{2}] {3}", domainName, date, level, message)
                        stderr.WriteLine(msg)
                        File.AppendAllLines(logFile, [| msg |])
        }

    let invalidation = Event<EventHandler,EventArgs>()
    let dispose () = ()
    let byteCache = Dictionary()
    let fileCache = Dictionary()

    let doMakeType (className: string []) (typeScriptFile: string) : Type =
        log.Information("TP.doMakeType({0}, {1})", String.concat "." className, typeScriptFile)
        let nameSpace = Array.sub className 0 (className.Length - 1)
        let typeName = className.[className.Length - 1]
        let compiler = Compiler()
        compiler.Log <- log
        compiler.TypeName <- typeName
        compiler.Namespace <- String.concat "." nameSpace
        match compiler.Compile(typeScriptFile) with
        | None -> failwith "Compilation failed"
        | Some assembly ->
            let bytes =
                use writer = new MemoryStream()
                assembly.Write(writer)
                writer.ToArray()
            let result =
                log.Time "Assembly.Load()" <| fun () ->
                    Assembly.Load(bytes)
            lock root <| fun () ->
                byteCache.[result.FullName] <- bytes
            result.GetType(assembly.EntryPoint)

    let makeType (className: string[]) (typeScriptFile: string) : Type =
        lock root <| fun () ->
            let key = (String.concat "." className, typeScriptFile)
            match fileCache.TryGetValue(key) with
            | true, t -> t
            | _ ->
                let t = doMakeType className typeScriptFile
                fileCache.[key] <- t
                t

    static let generator =
        let fullName = "IntelliFactory.TypeScript.Generator"
        let aB = AppDomain.CurrentDomain.DefineDynamicAssembly(AssemblyName(fullName), AssemblyBuilderAccess.Run)
        let mB = aB.DefineDynamicModule(fullName, false)
        let tB = mB.DefineType(fullName, TypeAttributes.Public ||| TypeAttributes.Class ||| TypeAttributes.Sealed)
        tB.CreateType()

    let ns =
        {
            new IProvidedNamespace with
                override this.GetNestedNamespaces() = Array.empty
                override this.GetTypes() = [| generator |]
                override this.ResolveTypeName(name) = generator.Assembly.GetType(name)
                override this.NamespaceName = generator.Namespace
        }

    let file =
        {
            new ParameterInfo() with
                override this.Attributes = ParameterAttributes.None
                override this.DefaultValue = null
                override this.Name = "file"
                override this.ParameterType = typeof<string>
                override this.Position = 0
                override this.RawDefaultValue = null
        }

    interface IDisposable with
        member this.Dispose() = dispose ()

    interface ITypeProvider with

        member this.ApplyStaticArguments(t: Type, className: string[], args: obj []) : Type =
            makeType className (args.[0] :?> _)

        member this.GetGeneratedAssemblyContents(a: Assembly) : byte[] =
            let bytes =
                lock root <| fun () ->
                    match byteCache.TryGetValue(a.FullName) with
                    | true, bytes -> Some bytes
                    | _ -> None
            match bytes with
            | None -> File.ReadAllBytes(a.ManifestModule.FullyQualifiedName)
            | Some bytes -> bytes

        member this.GetInvokerExpression(mb: MethodBase, par: Expr []) : Expr =
            let par = Array.toList par
            if mb.IsConstructor then
                Expr.NewObject(mb :?> ConstructorInfo, par)
            else
                let m = mb :?> MethodInfo
                if m.IsStatic
                    then Expr.Call(m, par)
                    else Expr.Call(par.Head, m, par.Tail)

        member this.GetNamespaces() =
            [| ns |]

        member this.GetStaticParameters(t: Type) : ParameterInfo [] =
            if t = generator then [| file |] else [||]

        [<CLIEvent>]
        member this.Invalidate = invalidation.Publish

[<assembly:TypeProviderAssembly>]
do ()
