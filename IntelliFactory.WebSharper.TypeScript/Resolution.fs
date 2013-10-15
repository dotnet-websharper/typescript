namespace IntelliFactory.WebSharper.TypeScript

/// Facilitates resolving TypeScript names.
module Resolution =
    module S = Syntax
    type AD = S.AmbientDeclaration
    type AMD = S.AmbientModuleDeclaration
    type DE = S.DeclarationElement
    type D = S.Declaration
    type MM = S.ModuleMember

    type ExternalModulePath =
        string

    type ModulePath =
        | GlobalModule
        | ExternalModule of ExternalModulePath
        | InnerModule of ModulePath * S.Identifier

    let innerModule mp i =
        InnerModule (mp, i)

    let rec subModulePath mp x =
        match x with
        | S.ModuleId i -> innerModule mp i
        | S.ModuleSubName (p, i) -> innerModule (subModulePath mp p) i

    type TypePath =
        | TypeAt of ModulePath * S.Identifier

    let makeTypePath mp id =
        TypeAt (mp, id)

    type ModuleEnvironment =
        {
            CurrentModulePath : ModulePath
            TryResolve : S.Identifier -> option<ModuleEnvironment>
        }

    let makeEnv path =
        {
            CurrentModulePath = path
            TryResolve = fun _ -> None
        }

    let rec tryResolveName me mn =
        match mn with
        | S.ModuleId id -> me.TryResolve(id)
        | S.ModuleSubName (p, i) ->
            match tryResolveName me p with
            | None -> None
            | Some r -> r.TryResolve(i)

    let delayEnv path (f: unit -> ModuleEnvironment) =
        let self =
            Lazy.Create(fun () ->
                let r = f ()
                if r.CurrentModulePath <> path then
                    failwith "Invalid module path in delayed environment"
                r)
        {
            makeEnv path with
                TryResolve = fun id ->
                    self.Value.TryResolve(id)
        }

    let resolveModuleName me mn =
        tryResolveName me mn
        |> Option.map (fun r -> r.CurrentModulePath)

    let resolveTypeName me tn =
        match tn with
        | S.TypeName.TypeId id ->
            Some (makeTypePath me.CurrentModulePath id)
        | S.TypeName.TypeSubName (mn, id) ->
            resolveModuleName me mn
            |> Option.map (fun r -> makeTypePath r id)

    let rec fixEnv path f =
        let rec self = delayEnv path (fun () -> f self)
        self

    let nestedEnv (a: ModuleEnvironment) (b: ModuleEnvironment) =
        match b.CurrentModulePath with
        | ModulePath.InnerModule (x, _) when x = a.CurrentModulePath -> ()
        | _ -> failwith "Invalid nested environment"
        {
            makeEnv b.CurrentModulePath with
                TryResolve = fun x ->
                    match b.TryResolve(x) with
                    | None -> a.TryResolve(x)
                    | r -> r
        }

    let tableEnv path xs =
        let d = dict xs
        {
            makeEnv path with
                TryResolve = fun x ->
                    let mutable r = Unchecked.defaultof<_>
                    if d.TryGetValue(x, &r) then Some r else None
        }

    /// Build environment for resolving names in the current AMD.
    let rec makeEnvForAMD (context: ModuleEnvironment) (amd: AMD) =
        let amdPath = innerModule context.CurrentModulePath amd.ModuleId
        fixEnv amdPath <| fun self ->
            seq {
                for el in amd.ModuleElements do
                    match el.Element with
                    | MM.ModuleNest subAMD ->
                        yield (subAMD.ModuleId, makeEnvForAMD self subAMD)
                    | MM.ModuleImport imp ->
                        let mN =
                            match imp.EntityName with
                            | S.EntityName.ModuleEntity mN -> S.ModuleId mN
                            | S.EntityName.InModuleEntity (mN, i) -> S.ModuleSubName (mN, i)
                        match tryResolveName self mN with
                        | Some r -> yield (imp.ImportIdentifier, r)
                        | _ -> ()
                    | _ -> ()
            }
            |> tableEnv amdPath
            |> nestedEnv context

    let visitInterface (env: ModuleEnvironment) (i: S.InterfaceDeclaration) =
        ()

    let visitDeclaration env (decl: S.Declaration) =
        match decl with
        | S.DeclareClass _ -> ()
        | S.DeclareEnum _ -> ()
        | S.DeclareFunction _ -> ()
        | S.DeclareVar _ -> ()

    let rec visitAMD (env: ModuleEnvironment) (amd: AMD) =
        for el in amd.ModuleElements do
            match el.Element with
            | MM.ModuleDeclare d ->
                visitDeclaration env d
            | MM.ModuleInterface i ->
                visitInterface env i
            | MM.ModuleNest subModule ->
                let subEnv = makeEnvForAMD env subModule
                visitAMD subEnv subModule
            | MM.ModuleImport _ -> ()

    let buildEnvForDSF (dsf: S.DeclarationSourceFile) =
        makeEnv ModulePath.GlobalModule

    let isExternalDSF (dsf: S.DeclarationSourceFile) =
        dsf.DeclarationElements
        |> Seq.exists (fun el ->
            match el with
            | DE.Ambient decl -> decl.Export
            | DE.Export expo -> true
            | DE.ExternalImport decl -> true
            | DE.Import decl -> decl.Export
            | DE.Interface decl -> decl.Export)

    let test (dsf: S.DeclarationSourceFile) =
        let env = buildEnvForDSF dsf
        for e in dsf.DeclarationElements do
            match e with
            | DE.Ambient exp ->
                match exp.Element with
                | AD.Declare _ -> ()
                | AD.DeclareExternalModule decl -> ()
                | AD.DeclareModule decl ->
                    visitAMD (makeEnvForAMD env decl) decl
            | DE.Export _ ->
                ()
            | DE.ExternalImport _ ->
                ()
            | DE.Import _ ->
                ()
            | DE.Interface i ->
                visitInterface env i.Element
