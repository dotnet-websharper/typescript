namespace IntelliFactory.TypeScript

open System
open System.IO
//open System.Reflection
//open IntelliFactory.Parsec
//module A = Assembling
//
//type IGeneratedAssembly =
//    abstract Write : Stream -> unit
//    abstract EntryPoint : string
//
///// Compiles TypeScript binding files to IBindings.
//[<Sealed>]
//type Compiler() =
//
//    static let (!) x = A.NetId.Create(x)
//    let mutable ns = A.NetName.Global(!"IntelliFactory").["TypeScript"]
//    let mutable tn = !"Extension"
//    let mutable log = Logging.Log.Default
//
//    static let parseQName (s: string) =
//        match Array.toList (s.Split([| '.' |], StringSplitOptions.RemoveEmptyEntries)) with
//        | [] -> A.NetName.Global(!"Unknown")
//        | x :: xs ->
//            (A.NetName.Global(!x), xs)
//            ||> List.fold (fun x y -> x.[y])
//
//    /// The log object to send warnings to.
//    member this.Log
//        with get () = log
//        and set x = log <- x
//
//    /// Sets up the namespace of the entry-point type.
//    member this.Namespace
//        with get () = string ns
//        and set x = ns <- parseQName x
//
//    /// Sets up the local name of the entry-point type.
//    member this.TypeName
//        with get () = string tn
//        and set x = tn <- A.NetId.Create(x)
//
//    /// Compiles a .d.ts file from the given location.
//    member this.Compile(declarationFile: string) : option<IGeneratedAssembly> =
//        let loc = Resolver.Location.Create declarationFile
//        let res = Parser.Parse log loc
//        match res with
//        | Parsed (syntax, pos, (), count) ->
//            let entryPoint = ns.[tn]
//            let assembly = Adaptation.Adapt log entryPoint syntax
//            let res = A.Assemble log assembly
//            Some {
//                new IGeneratedAssembly with
//                    member this.Write(s) = res.Write(s)
//                    member this.EntryPoint = res.EntryPoint
//            }
//        | ParseFailed e ->
//            log.Warning("Parse failed: {0}", e)
//            None
