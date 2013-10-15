namespace IntelliFactory.WebSharper.TypeScript

module FileSets =
    open System
    open System.Collections.Generic
    open System.IO
    open System.Text
    open FParsec
    module Names = IntelliFactory.WebSharper.TypeScript.Names
    module P = IntelliFactory.WebSharper.TypeScript.Parser
    module S = IntelliFactory.WebSharper.TypeScript.Syntax

    type Config =
        {
            NameBuilder : Names.NameBuilder
            ResolveModule : string -> string
        }

    type SourceFile =
        {
            ExternReferences : Set<string>
            FullPath : string
            KnownExternModules : Set<string>
            ReferencePaths : Set<string>
        }

    type FileSet =
        {
            SourceFiles : list<SourceFile>
        }

    let emptySourceFileSet =
        { SourceFiles = [] }

    let addSourceFile sf sfs =
        { sfs with SourceFiles = sf :: sfs.SourceFiles }

    let findFilesToInclude sfs =
        let includedFiles =
            sfs.SourceFiles
            |> Seq.map (fun f -> f.FullPath)
            |> Set.ofSeq
        let refPaths =
            sfs.SourceFiles
            |> Seq.collect (fun f -> f.ReferencePaths)
            |> Set.ofSeq
        refPaths - includedFiles

    let findExternModulesToInclude sfs =
        let extRefs =
            sfs.SourceFiles
            |> Seq.map (fun f -> f.ExternReferences)
            |> Set.unionMany
        let knownRefs =
            sfs.SourceFiles
            |> Seq.map (fun f -> f.KnownExternModules)
            |> Set.unionMany
        extRefs - knownRefs

    let enc = UTF8Encoding(false, true)

    let findKnownExternModules (dsf: S.DeclarationSourceFile) =
        seq {
            for el in dsf.DeclarationElements do
                match el with
                | S.Ambient { Element = S.DeclareExternalModule em } ->
                    yield em.ExternalModulePath
                | _ -> ()
        }

    let findExternImports (dsf: S.DeclarationSourceFile) =
        seq {
            for el in dsf.DeclarationElements do
                match el with
                | S.ExternalImport { Element = S.ImportRequire (_, path) } ->
                    yield path
                | S.Ambient { Element = S.DeclareExternalModule em } ->
                    for el in em.ExternalModuleElements do
                        match el with
                        | S.Import { Element = S.ImportRequire (_, path) } ->
                            yield path
                        | _ -> ()
                | _ -> ()
        }

    let readSourceFile cfg (fullPath: string) =
        let fullPath = FileInfo(fullPath).FullName
        let st = P.UserState.Create(cfg.NameBuilder)
        match runParserOnFile P.DeclarationSourceFile st fullPath enc with
        | Failure (expl, _, _) -> failwith expl
        | Success (res, st, pos) ->
            let bP = Path.GetDirectoryName(fullPath)
            let mkSet xs =
                xs
                |> Seq.distinct
                |> Set.ofSeq
            let knownExterns =
                findKnownExternModules res |> mkSet
            let extImports =
                findExternImports res |> mkSet
            let refPaths =
                seq {
                    for p in st.ReferencePaths do
                        yield Path.Combine(bP, p)
                    for i in extImports do
                        if i.StartsWith(".") then
                            yield Path.Combine(bP, i)
                }
                |> Seq.distinct
                |> Set.ofSeq
            let externReferences =
                let absImports =
                    extImports
                    |> Set.filter (fun i -> i.StartsWith(".") |> not)
                absImports - knownExterns
            {
                ExternReferences = externReferences
                FullPath = fullPath
                KnownExternModules = knownExterns
                ReferencePaths = refPaths
            }

    let LoadFileSet cfg file =
        let startingSet =
            emptySourceFileSet
            |> addSourceFile (readSourceFile cfg file)
        let addFiles f set xs =
            Seq.fold (fun s t -> addSourceFile (f t) s) set xs
        let rec loop set =
            let inc = findFilesToInclude set
            if inc.IsEmpty then
                let ext = findExternModulesToInclude set
                if ext.IsEmpty then set else
                    let p = cfg.ResolveModule >> readSourceFile cfg
                    loop (addFiles p set ext)
            else
                loop (addFiles (readSourceFile cfg) set inc)
        loop startingSet
