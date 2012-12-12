/// Implements parsing of TypeScript 0.8 definitions (.d.ts) files.
module IntelliFactory.TypeScript.Declarations.Parser

#nowarn "40"

module L = Lexer
module S = Syntax
open System.Collections.Generic
open System.IO
open IntelliFactory.Parsec
open IntelliFactory.TypeScript
module P = IntelliFactory.Parsec.Parser

type L<'T> = Parser<'T,string,char,unit,int>
type P<'T> = Parser<'T,L.Lexeme[],L.Lexeme,unit,int>

let ( <*> ) f x = P.Map2 (fun f x -> f x) f x
let ( <^> ) f x = P.Map f x
let ( <|> ) a b = P.Or a b
let ( >*> ) a b = P.Combine a b
let ( <*< ) a b = a |> P.Before b
let ( <=> ) a b = b >*> P.Yield a
let ( <?> ) a b = P.Label b a

let pPrim (f: L.Token -> option<'T>) : P<'T> =
    P.Token (fun l -> f l.Token)

let pToken (t: L.Token) : P<unit> =
    P.Satisfy (fun (l: L.Lexeme) -> l.Token = t)
    |> P.Ignore

let pKeyword (kw: L.Keyword) : P<unit> =
    P.Satisfy (fun (l: L.Lexeme) ->
        match l.Token with
        | L.TokenKeyword k -> k = kw
        | _ -> false)
    |> P.Ignore

let pIdentifier : P<S.Identifier> =
    let f x =
        match x with
        | L.TokenIdentifier x -> Some x
        | L.TokenKeyword w -> Some (S.Identifier (w.ToText()))
        | _ -> None
    pPrim f

let pString : P<string> =
    let f x =
        match x with
        | L.TokenStringLiteral x -> Some x
        | _ -> None
    pPrim f

let pBlock (p: P<'T>) : P<'T> =
    P.Between (pToken L.TokenCurlyOpen) (pToken L.TokenCurlyClose) p

let pBracketed (p: P<'T>) : P<'T> =
    P.Between (pToken L.TokenSquareOpen) (pToken L.TokenSquareClose) p

let pParens (p: P<'T>) : P<'T> =
    P.Between (pToken L.TokenParenOpen) (pToken L.TokenParenClose) p

let pComma : P<unit> =
    pToken L.TokenComma

let isImpliedSemicolon (l: L.Lexeme) : bool =
    match l.Options, l.Token with
    | L.NewLine, _
    | _, L.TokenCurlyClose -> true
    | _ -> false

let pSemi : P<unit> =
    let implied = P.LookAhead (P.Satisfy isImpliedSemicolon)
    (pToken L.TokenSemicolon <|> P.Ignore implied <|> P.EndOfInput) <?> ";"

let pName : P<S.Name> =
    pIdentifier
    |> P.SepBy1 (pToken L.TokenDot)
    |> P.Map (fun s ->
        let a = Seq.toArray s
        {
            Name = a.[a.Length - 1]
            Namespace =
                Array.sub a 0 (a.Length - 1)
                |> List.ofArray
        })

let pRequirement : P<S.Requirement> =
    (S.Optional <=> pToken L.TokenQuestion)
    <|> P.Yield S.Required

(* See 3.5, 6.2 *)

let pPredefinedType : P<S.Type> =
    let ( ==> ) a b = b <=> pKeyword a
    (L.KeywordAny ==> S.AnyType)
    <|> (L.KeywordBool ==> S.BoolType)
    <|> (L.KeywordNumber ==> S.NumberType)
    <|> (L.KeywordString ==> S.StringType)

type Parameter =
    | Optional of S.Parameter
    | Required of S.Parameter
    | Rest of S.Parameter

let rec pType : P<S.Type> =
    P.Delay <| fun () ->
        let mk t bs = Seq.fold (fun t _ -> S.ArrayOf t) t bs
        let brackets =
            P.Many (pBracketed (P.Yield ()))
        let p =
            pPredefinedType
            <|> pTypeLiteral
            <|> (S.TypeName <^> pName)
        mk <^> p <*> brackets

and pTypeAnnotation : P<S.Type> =
    P.WithDefault S.AnyType (pToken L.TokenColon >*> pType)

and pReturnType : P<S.Return> =
    (S.Void <=> pKeyword L.KeywordVoid)
    <|> (S.Returns <^> pType)

and pReturnTypeAnnotation : P<S.Return> =
    (pToken L.TokenColon >*> pReturnType)
    |> P.WithDefault (S.Returns S.AnyType)

and pRequiredParameter : P<S.Parameter> =
    (fun x y -> S.Parameter (x, y))
    <^> pIdentifier
    <*> pTypeAnnotation

and pParameters : P<S.Parameters> =
    let pPar =
        (fun rest id req ty ->
            match rest, req with
            | true, _ ->
                let ty =
                    match ty with
                    | S.ArrayOf _ -> ty
                    | _ -> S.ArrayOf ty
                Rest (S.Parameter (id, ty))
            | _, S.Optional -> Optional (S.Parameter (id, ty))
            | _, _ -> Required (S.Parameter (id, ty)))
        <^> P.WithDefault false (true <=> pToken L.TokenEllipsis)
        <*> pIdentifier
        <*> pRequirement
        <*> pTypeAnnotation
    pParens (P.SepBy pComma pPar)
    |> P.Map (fun ps ->
        let req = Queue()
        let opt = Queue()
        let rest = ref None
        for p in ps do
            match p with
            | Optional p -> opt.Enqueue(p)
            | Required p -> req.Enqueue(p)
            | Rest p -> rest := Some p
        {
            OptionalParameters = (opt.ToArray()) :> seq<_>
            RequiredParameters = (req.ToArray()) :> seq<_>
            RestParameter = !rest
        })

and pCallSignature : P<S.CallSignature> =
    (fun x y -> S.CallSignature (x, y))
    <^> pParameters
    <*> pReturnTypeAnnotation

and pConstructSignature : P<S.ConstructSignature> =
    (fun x y -> S.ConstructSignature (x, y))
    <^> (pKeyword L.KeywordNew >*> pParameters)
    <*> pTypeAnnotation

and pIndexSignature : P<S.IndexSignature> =
    (fun x y -> S.IndexSignature (x, y))
    <^> pBracketed pRequiredParameter
    <*> pTypeAnnotation

and pPropertySignature : P<S.PropertySignature> =
    (fun x y z -> S.PropertySignature(x, y, z))
    <^> pIdentifier
    <*> pRequirement
    <*> pTypeAnnotation

and pFunctionSignature : P<S.FunctionSignature> =
    pIdentifier
    |> P.Map (fun a b c d -> S.FunctionSignature (a, b, c, d))
    |> P.WithDefault (fun x y z -> S.FunctionCallSignature (x, y, z))
    <*> pRequirement
    <*> pParameters
    <*> pReturnTypeAnnotation

and pTypeMember : P<S.TypeMember> =
    (S.CallMember <^> pCallSignature)
    <|> (S.ConstructMember <^> pConstructSignature)
    <|> (S.IndexMember <^> pIndexSignature)
    <|> P.Try (S.FunctionMember <^> pFunctionSignature)
    <|> (S.PropertyMember <^> pPropertySignature)

and pObjectType : P<seq<S.TypeMember>> =
    pBlock (P.SepEndBy pSemi pTypeMember)

and pConstructorType : P<S.ConstructSignature> =
    (fun x y -> S.ConstructSignature (x, y))
    <^> (pKeyword L.KeywordNew >*> pParameters)
    <*> (pToken L.TokenDoubleArrow >*> pType)

and pFunctionType : P<S.CallSignature> =
    (fun x y -> S.CallSignature (x, y))
    <^> pParameters
    <*> (pToken L.TokenDoubleArrow >*> pReturnType)

and pTypeLiteral : P<S.Type> =
    let mkFun f = S.ObjectType [| S.CallMember f |]
    let mkCtor c = S.ObjectType [| S.ConstructMember c |]
    (S.ObjectType <^> pObjectType)
    <|> (mkCtor <^> pConstructorType)
    <|> (mkFun <^> pFunctionType)

let pVisibility : P<S.Visibility> =
    (S.Public <=> pKeyword L.KeywordPublic)
    <|> (S.Private <=> pKeyword L.KeywordPrivate)
    <|> P.Yield S.Default

/// See 7.1
let pInterfaceDeclaration : P<S.InterfaceDeclaration> =
    (fun name ext ms ->
        {
            ExtendedInterfaces = ext
            InterfaceMembers = ms
            Name = name
        })
    <^> (pKeyword L.KeywordInterface >*> pIdentifier)
    <*> (P.WithDefault Seq.empty (pKeyword L.KeywordExtends >*> P.SepBy1 pComma pName))
    <*> pObjectType

/// See 9.2.2
let pImportDeclaration : P<S.ImportDeclaration> =
    let pIntern = Choice2Of2 <^> pName
    let pExtern = Choice1Of2 <^> (pKeyword L.KeywordModule >*> pParens pString)
    let mk i x =
        match x with
        | Choice1Of2 x -> S.ExternalImport (i, x)
        | Choice2Of2 x -> S.InternalImport (i, x)
    mk
    <^> (pKeyword L.KeywordImport >*> pIdentifier <*< pToken L.TokenEquals)
    <*> (P.Try pExtern <|> pIntern)

(* See 10.1.3 *)

let pAmbientConstructorDeclaration : P<S.ClassMember> =
    S.Constructor
    <^> P.Between (pKeyword L.KeywordConstructor) pSemi pParameters

let pAmbientMemberDeclaration : P<S.ClassMember> =
    let mk v x =
        match x with
        | Choice1Of2 f -> S.InstanceField (v, f)
        | Choice2Of2 f -> S.InstanceFunction (v, f)
    let mm =
        P.Try (Choice2Of2 <^> pFunctionSignature)
        <|> (Choice1Of2 <^> pRequiredParameter)
    mk <^> pVisibility <*> mm <*< pSemi

let pAmbientStaticDeclaration : P<S.ClassMember> =
    let field = S.StaticField <^> pRequiredParameter
    let func = S.StaticFunction <^> pFunctionSignature
    (P.Try func <|> field)
    |> P.Between (pKeyword L.KeywordStatic) pSemi

let pAmbientClassBodyElement : P<S.ClassMember> =
    pAmbientConstructorDeclaration
    <|> pAmbientStaticDeclaration
    <|> pAmbientMemberDeclaration

let pClassHeritage : P<option<S.Name> * seq<S.Name>> =
    (fun ext impl -> (ext, impl))
    <^> P.Option (pKeyword L.KeywordExtends >*> pName)
    <*> P.WithDefault Seq.empty
        (pKeyword L.KeywordImplements >*> P.SepBy1 pComma pName)

let pAmbientClassDeclaration : P<S.AmbientClassDeclaration> =
    (fun i (inh, impl) mem ->
        {
            ClassMembers = mem
            Implements = impl
            Inherits = inh
            Name = i
        })
    <^> (pKeyword L.KeywordClass >*> pIdentifier)
    <*> pClassHeritage
    <*> pBlock (P.Many pAmbientClassBodyElement)

(* Extra: Enums *)

let pEnumDeclaration : P<S.EnumDeclaration> =
    (fun x y -> S.EnumDeclaration (x, y))
    <^> (pKeyword L.KeywordEnum >*> pIdentifier)
    <*> pBlock (P.SepBy1 pComma pIdentifier)

(* See 10.1.4 *)

let pAmbientFunctionDeclaration : P<S.FunctionSignature> =
    P.Between (pKeyword L.KeywordFunction) pSemi pFunctionSignature

let pAmbientVariableDeclaration : P<S.Parameter> =
    P.Between (pKeyword L.KeywordVar) pSemi pRequiredParameter

let pGenericDeclaration p =
    let flags = pKeyword L.KeywordExport <|> pKeyword L.KeywordDeclare
    P.SkipMany flags >*> p <*< P.SkipMany (pToken L.TokenSemicolon)

let pNonModuleElement : P<S.ModuleElement> =
    (S.ClassElement <^> pAmbientClassDeclaration)
    <|> (S.FunctionElement <^> pAmbientFunctionDeclaration)
    <|> (S.VariableElement <^> pAmbientVariableDeclaration)
    <|> (S.InterfaceElement <^> pInterfaceDeclaration)
    <|> (S.EnumElement <^> pEnumDeclaration)
    <|> (S.ImportElement <^> pImportDeclaration)

let rec pInternalModule : P<S.InternalModule> =
    P.Delay <| fun () ->
        (fun n x -> { Name = n; Members = x })
        <^> (pKeyword L.KeywordModule >*> pName)
        <*> pBlock (P.Many pModuleElement)

and pModuleElement : P<S.ModuleElement> =
    let p = pNonModuleElement <|> (S.ModuleElement <^> pInternalModule)
    pGenericDeclaration p

let pExternalModule : P<S.ExternalModule> =
    (fun n x -> { Path = n; Members = x })
    <^> (pKeyword L.KeywordModule >*> pString)
    <*> pBlock (P.Many pModuleElement)

let pDeclaration : P<S.Declaration> =
    let p =
        (S.RegularDeclaration <^> pNonModuleElement)
        <|> P.Try (S.ExternalModuleDeclaration <^> pExternalModule)
        <|> (S.RegularDeclaration <^> pModuleElement)
    pGenericDeclaration p

/// See 10.2
let pDeclarationSourceFile : P<S.DeclarationSourceFile> =
    S.DeclarationSourceFile <^> P.Many pDeclaration <*< P.EndOfInput

(* Public *)

let getPosition (lexeme: L.Lexeme) =
    lexeme.SourcePosition

let Parse log loc : ParseResult<S.DeclarationSourceFile,unit,int> =
    let a = Resolver.Resolve log loc
    printfn "TOKENS: %i" a.Length
    a
    |> ParseInput.FromLexedArray () (string loc) getPosition
    |> pDeclarationSourceFile.Parse
