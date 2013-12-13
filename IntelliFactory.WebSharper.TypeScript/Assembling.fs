/// Constructs .NET assemblies, includes a built-in algorithm for name disambiguation.
module internal IntelliFactory.TypeScript.Assembling

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Text
open System.Text.RegularExpressions
//module A = IntelliFactory.WebSharper.Core.Attributes
//module M = Memoization
//module Mp = Mappings
//module S = Syntax
//
//type NetId = Symbols.Symbol<Symbols.NetChecker>
//type NetName = Symbols.Name<Symbols.NetChecker>
//
//module private Identifiers =
//    let Call = NetId.Create("Call")
//    let Item = NetId.Create("Item")
//    let New = NetId.Create("New")
//
//    module Syntax =
//        let Call = S.Identifier.Create("Call")
//        let Item = S.Identifier.Create("Item")
//        let New = S.Identifier.Create("New")
//
//type Key =
//    | Call
//    | ItemByNumber
//    | ItemByString
//    | New
//    | Property of S.Identifier
//
//    member this.Id =
//        match this with
//        | Call -> Identifiers.Syntax.Call
//        | ItemByNumber | ItemByString -> Identifiers.Syntax.Item
//        | New -> Identifiers.Syntax.New
//        | Property p -> p
//
//    member this.NetId() =
//        match this with
//        | Call -> Identifiers.Call
//        | ItemByNumber | ItemByString -> Identifiers.Item
//        | New -> Identifiers.New
//        | Property p -> Symbols.ConvertSymbol p
//
//[<Sealed>]
//type Contract(def: ContractDefinition) =
//    member this.AllowsConstructor = def.AllowsConstructor
//    member this.Definition = def
//    member this.HintName = def.HintName
//    member this.Properties = def.Properties
//    member this.Suffix = def.Suffix
//
//and ContractDefinition =
//    {
//        AllowsConstructor : bool
//        HintName : NetName
//        Properties : Mp.Mapping<Key,Member>
//        Suffix : NetId
//    }
//
//and Member =
//    | OverloadedMember of Overloads
//    | TypedMember of Type
//
//and [<Sealed>] Overloads(orig: list<Signature * Type>) =
//    let variants =
//        lazy
//            orig
//            |> Seq.distinctBy fst
//            |> Seq.toList
//
//    member this.IsEmpty = orig.IsEmpty
//    member this.Variants = variants.Value
//    override this.ToString() =
//        sprintf "+%A" this.Variants
//
//and Signature =
//    Signatures.Signature<Type>
//
//and Type =
//    | AnyType
//    | ArrayType of Type
//    | BooleanType
//    | ContractType of Lazy<Contract>
//    | FunctionType of list<Type> * Type
//    | NumberType
//    | StringType
//    | UnitType
//
//[<Sealed>]
//type Singleton(hintName: NetName, synName: S.Name, singleton: Member, suffix: NetId) =
//    member this.HintName = hintName
//    member this.Member = singleton
//    member this.Suffix = suffix
//    member this.SyntaxName = synName
//
///// Picking names that do not conflict based on name hints.
//[<AutoOpen>]
//module private Diambiguation =
//
//    type NetNameHint =
//        {
//            Namespace : NetName
//            Name : NetId
//            Suffix : option<NetId>
//        }
//
//        static member Create(name: NetName, suffix: NetId) : NetNameHint =
//            match name.Parent with
//            | None ->
//                {
//                    Namespace = name
//                    Name = suffix
//                    Suffix = None
//                }
//            | Some ns ->
//                {
//                    Namespace = ns
//                    Name = name.Local
//                    Suffix = Some suffix
//                }
//
//    let private MakeName (hint: NetNameHint) (attempt: int) : NetName =
//        match attempt with
//        | 0 -> hint.Namespace.[hint.Name]
//        | k ->
//            let suffix =
//                match hint.Suffix with
//                | Some x ->
//                    match k with
//                    | 1 -> string x
//                    | _ -> string x + string (k - 1)
//                | None ->
//                    string k
//            hint.Namespace.[hint.Name.Name + suffix]
//
//    let private PickName (used: HashSet<NetName>) (hint: NetNameHint) : NetName =
//        let rec pick n =
//            let name = MakeName hint n
//            if used.Add(name) then
//                name
//            else
//                pick (n + 1)
//        pick 0
//
//    let Disambiguate<'T> (used: seq<NetName>) (hint: 'T -> NetNameHint) (input: seq<'T>) : Mp.Mapping<NetName,'T> =
//        let input = Seq.toArray input
//        let hints = Array.map hint input
//        let namespaceNames =
//            hints
//            |> Seq.map (fun x -> x.Namespace)
//        let usedNames = HashSet(Seq.append used namespaceNames)
//        let pairs =
//            input
//            |> Seq.mapi (fun i x -> (PickName usedNames hints.[i], x))
//        Mp.Mapping(pairs)
//
//    let private MakeId (hint: NetId) (attempt: int) : NetId =
//        match attempt with
//        | 0 -> hint
//        | k ->
//            let suffix = string attempt
//            NetId.Create(string hint + string suffix)
//
//    let private PickId (used: HashSet<NetId>) (hint: NetId) : NetId =
//        let rec pick n =
//            let name = MakeId hint n
//            if used.Add(name) then
//                name
//            else
//                pick (n + 1)
//        pick 0
//
//    let DisambiguateLocal<'T> (inUse: seq<NetId>) (hint: 'T -> NetId) (input: seq<'T>) : Mp.Mapping<NetId,'T> =
//        let used = HashSet(inUse)
//        let pairs =
//            input
//            |> Seq.map (fun x -> (PickId used (hint x), x))
//        Mp.Mapping(pairs)
//
//type AssemblyDefinition =
//    {
//        Contracts : seq<Contract>
//        Singletons : seq<Singleton>
//        Start : NetName
//    }
//
//type private NamedDefinition =
//    | NamedContract of Contract * NetName * Mp.Mapping<Key,NetId>
//    | NamedSingleton of Singleton * NetName
//
///// Disambiguates names and constructs name mappings.
//[<Sealed>]
//type private NamedAssembly private (entryPoint: NetName, defs: seq<NamedDefinition>) =
//
//    let contractNames =
//        defs
//        |> Seq.choose (function
//            | NamedContract (c, n, _) -> Some (c, n)
//            | _ -> None)
//        |> dict
//
//    let singletonNames =
//        defs
//        |> Seq.choose (function
//            | NamedSingleton (c, n) -> Some (c, n)
//            | _ -> None)
//        |> dict
//
//    member this.Name(c: Contract) = contractNames.[c]
//    member this.Name(s: Singleton) = singletonNames.[s]
//    member this.Definitions = defs
//    member this.EntryPoint = entryPoint
//
//    static member Create(def: AssemblyDefinition) : NamedAssembly =
//        let hintContract (c: Contract) : NetNameHint =
//            NetNameHint.Create(c.HintName, c.Suffix)
//        let hintSingleton (s: Singleton) : NetNameHint =
//            NetNameHint.Create(s.HintName, s.Suffix)
//        let entryPoint = def.Start
//        let defs =
//            Seq.append
//                (Seq.map Choice1Of2 def.Contracts)
//                (Seq.map Choice2Of2 def.Singletons)
//            |> Disambiguate [entryPoint] (function
//                | Choice1Of2 x -> hintContract x
//                | Choice2Of2 x -> hintSingleton x)
//            |> Mp.GetPairs
//            |> Seq.map (fun (name, entity) ->
//                match entity with
//                | Choice1Of2 contract ->
//                    let used = name.List
//                    let contractMap =
//                        contract.Properties.GetKeys()
//                        |> DisambiguateLocal used (fun x -> x.NetId())
//                        |> Mp.GetPairs
//                        |> Seq.map (fun (a, b) -> (b, a))
//                        |> Mp.New
//                    NamedContract (contract, name, contractMap)
//                | Choice2Of2 singleton ->
//                    NamedSingleton (singleton, name))
//            |> Seq.cache
//        NamedAssembly(entryPoint, defs)
//

//

//let Assemble (log: Logging.Log) (def: AssemblyDefinition) : GeneratedAssembly =
//    let bytes =
//        NamedAssembly.Create(def)
//        |> Compilation.Compile
//        |> Compilation.Construct log (AssemblyName(def.Start.Text))
//    GeneratedAssembly(bytes, def.Start)
