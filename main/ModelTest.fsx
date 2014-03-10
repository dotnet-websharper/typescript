#r "../build/net40/IntelliFactory.WebSharper.TypeScript.dll"

open IntelliFactory.WebSharper.TypeScript
module SFD = SourceFileDependencies

type Config =
    {
        TypeScriptDeclarationFiles : seq<string>
    }

let GetSourceFileSet nb logger cfg =
    /// TODO: perhaps warn if no input files found.
    {
        SFD.Configure nb logger cfg.TypeScriptDeclarationFiles with
            Resolver = SFD.Resolver.Failure // TODO
    }
    |> SFD.Resolve

let Test (file: string) =
    let file = System.IO.Path.Combine(__SOURCE_DIRECTORY__, file)
    let nb = Names.NameBuilder.Create()
    let logger = Logger(Logging.Level.Verbose)
    let set = GetSourceFileSet nb logger { TypeScriptDeclarationFiles = [file] }
    for msg in logger.All do
        printfn "MSG: %A" msg
    let v = Model.Visit()
    for file in set.SourceFiles do
        printfn "File: %s" file.FilePath
        v.SourceFile(file)

(*
    Test "test.d.ts"
*)
