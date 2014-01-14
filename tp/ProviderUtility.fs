// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

namespace IntelliFactory.WebSharper.TypeScript

[<AutoOpen>]
module internal ProviderUtility =

    let MemoizeBy (getKey: 'A -> 'K) (build: 'A -> 'B) : ('A -> 'B) =
        let hasher x = hash (getKey x)
        let eq x y = getKey x = getKey y
        let comp = HashIdentity.FromFunctions hasher eq
        let cache = ConcurrentDictionary<'A,'B>(comp)
        fun a -> cache.GetOrAdd(a, build)

    let CreateGeneratorType (fullName: string) =
        let aB = AppDomain.CurrentDomain.DefineDynamicAssembly(AssemblyName(fullName), AssemblyBuilderAccess.Run)
        let mB = aB.DefineDynamicModule(fullName, false)
        let tB = mB.DefineType(fullName, TypeAttributes.Public ||| TypeAttributes.Class ||| TypeAttributes.Sealed)
        tB.CreateType()

    type ParamSpec =
        | OptionalParam of string * Type * obj
        | RequiredParam of string * Type

    let CreateParameterInfos specs =
        specs
        |> Seq.mapi (fun i par ->
            let (name, ty, def) =
                match par with
                | OptionalParam (name, ty, v) -> (name, ty, Some v)
                | RequiredParam (name, ty) -> (name, ty, None)
            let attrs =
                match def with
                | None -> ParameterAttributes.None
                | Some _ -> ParameterAttributes.Optional
            let def = defaultArg def null
            {
                new ParameterInfo() with
                    override this.Attributes = attrs
                    override this.DefaultValue = def
                    override this.Name = name
                    override this.ParameterType = ty
                    override this.Position = i
                    override this.RawDefaultValue = def
            })
        |> Seq.toArray

    let CreateProvidedNamespace (generatorType: Type) =
        {
            new IProvidedNamespace with
                override this.GetNestedNamespaces() = Array.empty
                override this.GetTypes() = [| generatorType |]
                override this.ResolveTypeName(name) = generatorType.Assembly.GetType(name)
                override this.NamespaceName = generatorType.Namespace
        }

    let WithTempFile tempDir ext f =
        let t = Path.Combine(tempDir, Guid.NewGuid().ToString() + ext)
        try
            f t
        finally
            if File.Exists(t) then
                File.Delete(t)

    let GetGeneratedAssemblyContents tempDir (assem: Assembly) =
        match assem with
        | :? AssemblyBuilder as builder ->
            WithTempFile tempDir ".dll" <| fun tempFile ->
                builder.Save(tempFile)
                File.ReadAllBytes(tempFile)
        | _ ->
            invalidArg "assem" ("Not an AssemblyBuilder: " + string assem.FullName)

    type AssemblyConfig =
        {
            ClassName : string
            ParameterValues : obj []
        }

    type Config =
        {
            BuildAssembly : AssemblyConfig -> byte []
            GeneratorParameters : list<ParamSpec>
            GeneratorTypeName : string
            TypeProviderConfig : TypeProviderConfig
        }

    let DefineProvider (cfg: Config) =
        let config = cfg.TypeProviderConfig
        let genType = CreateGeneratorType cfg.GeneratorTypeName
        let genNS = CreateProvidedNamespace genType
        let genParams = CreateParameterInfos cfg.GeneratorParameters
        let invalidation = Event<EventHandler,EventArgs>()
        let contents = ConcurrentDictionary<string,byte[]>()
        {
            new ITypeProvider with

                member this.ApplyStaticArguments(t, className, args) =
                    let className = String.concat "." className
                    printfn "ApplyStaticArguments to %s" className
                    let bytes =
                        cfg.BuildAssembly {
                            ClassName = className
                            ParameterValues = args
                        }
                    let asm = Assembly.Load(bytes)
                    contents.AddOrUpdate(asm.FullName, bytes, fun _ _ -> bytes) |> ignore
                    asm.GetType(className)

                member this.GetGeneratedAssemblyContents(assem) =
                    contents.[assem.FullName]

                /// Default implementation: no quotation-level rewriting.
                member this.GetInvokerExpression(mb, par) =
                    printfn "GetInvokerExpression"
                    let par = Array.toList par
                    if mb.IsConstructor then
                        Expr.NewObject(mb :?> ConstructorInfo, par)
                    else
                        let m = mb :?> MethodInfo
                        if m.IsStatic
                            then Expr.Call(m, par)
                            else Expr.Call(par.Head, m, par.Tail)

                member this.GetNamespaces() =
                    [| genNS |]

                member this.GetStaticParameters(t: Type) =
                    if t = genType then genParams else Array.empty

                [<CLIEvent>]
                member this.Invalidate =
                    invalidation.Publish

            interface IDisposable with
                member this.Dispose() = ()
        }

    /// See http://stackoverflow.com/questions/10357273/type-provider-calling-another-dll-in-f
    let InstallAssemblyResolver () =
        AppDomain.CurrentDomain.add_AssemblyResolve(fun _ args ->
            let name = AssemblyName(args.Name)
            let existingAssembly =
                AppDomain.CurrentDomain.GetAssemblies()
                |> Seq.tryFind(fun a ->
                    AssemblyName.ReferenceMatchesDefinition(name, a.GetName()))
            defaultArg existingAssembly null)
