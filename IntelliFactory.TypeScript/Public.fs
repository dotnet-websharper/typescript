namespace IntelliFactory.TypeScript

open IntelliFactory.Parsec
open System.IO

/// Represents compiled WebSharper declarations.
[<Sealed>]
type CompiledDeclarations internal (ns: CSharp.Namespace) =

    /// Writes C# code to the given writer.
    member this.Write(out: TextWriter) =
        CSharp.Compile out ns

    /// Writes C# code to the given file.
    member this.WriteFile(file: string) =
        File.WriteAllText(file, this.WriteString())

    /// Writes C# code to a string.
    member this.WriteString() =
        use w = new StringWriter()
        this.Write(w)
        w.ToString()

[<Sealed>]
type Compiler() =
    let mutable ns : seq<string> = Seq.ofList ["IntelliFactory"; "WebSharper"]
    let mutable tn = "Extension"
    let mutable log = Log.Default

    member this.Log
        with get () = log
        and set x = log <- x

    member this.Namespace
        with get () = ns
        and set x = ns <- x

    member this.TypeName
        with get () = tn
        and set x = tn <- x

    member this.Compile(declarationFile: string) =
        let loc = Declarations.Resolver.Location.Create declarationFile
        let res = Declarations.Parser.Parse log loc
        match res with
        | Parsed (x, _, _, _) ->
            let csn = Declarations.Builder.Build this.Namespace this.TypeName x
            Some (CompiledDeclarations(csn))
        | ParseFailed e ->
            log.Warn("Parse failed: {0}", e)
            None

