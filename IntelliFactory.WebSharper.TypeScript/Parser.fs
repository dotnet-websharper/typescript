
namespace IntelliFactory.WebSharper.TypeScript

#nowarn "40"

module Parser =
    open System
    open FParsec
    module Lexer = IntelliFactory.WebSharper.TypeScript.Lexer
    module S = IntelliFactory.WebSharper.TypeScript.Syntax
    type UserState = Lexer.UserState
    type Parser<'T> = Parser<'T,Lexer.UserState>
    type P<'T> = Parser<'T>

    type Grammar =
        {
            AmbientClassDeclaration : P<S.AmbientClassDeclaration>
            AmbientDeclaration : P<S.AmbientDeclaration>
            AmbientExternalModuleDeclaration : P<S.AmbientExternalModuleDeclaration>
            AmbientModuleDeclaration : P<S.AmbientModuleDeclaration>
            CallSignature : P<S.CallSignature>
            ConstructSignature : P<S.CallSignature>
            ConstructorType : P<S.Type>
            Declaration : P<S.Declaration>
            FunctionType : P<S.Type>
            ImportDeclaration : P<S.ImportDeclaration>
            IndexSignature : P<S.IndexSignature>
            InterfaceDeclaration : P<S.InterfaceDeclaration>
            MethodSignature : P<S.TypeMember>
            ModuleMember : P<S.ModuleMember>
            ObjectType : P<list<S.TypeMember>>
            Params : P<S.Parameters>
            PropertySignature : P<S.PropertySignature>
            Type : P<S.Type>
            TypeAnnot : P<S.Type>
            TypeAnnotation : P<S.Type>
            TypeBody : P<list<S.TypeMember>>
            TypeArgs : P<list<S.Type>>
            TypeMember : P<S.TypeMember>
            TypeParameter : P<S.TypeParameter>
            TypeParams : P<list<S.TypeParameter>>
            TypeReference : P<S.TypeReference>
        }

    let gFix (f: P<'T> -> P<'T>) : P<'T> =
        let rec p =
            let r = lazy f p
            parse { return! r.Value }
        p

    /// TODO: non-integer numeric literals.
    let gPropertyName : P<S.PropertyName> =
        choice [
            Lexer.IdentifierName |>> fun x -> S.PropertyName.Create(x.Text)
            Lexer.StringLiteral |>> S.PropertyName.Create
            Lexer.IntegerLiteral |>> (string >> S.PropertyName.Create)
        ]

    let gIdentName = Lexer.IdentifierName
    let gIdent = Lexer.Identifier

    let gSemi  = Lexer.``;``
    let gStringLiteral = Lexer.StringLiteral

    let gChoice2 a b =
        (a |>> Choice1Of2) <|> (b |>> Choice2Of2)

    let gDot = Lexer.``.``
    let gComma = Lexer.``,``
    let gAngular p = between Lexer.LessThan Lexer.GreaterThan p
    let gCurly p = between Lexer.``{`` Lexer.``}`` p
    let gParen p = between Lexer.``(`` Lexer.``)`` p

    let gIdentifierPath : P<list<S.Identifier>> =
        sepBy1 Lexer.Identifier Lexer.``.``

    let defTypeParameter g : P<S.TypeParameter> =
        pipe2 gIdent (opt (Lexer.Extends >>. g.Type)) <| fun id c ->
            { Identifier = id; Extends = c }

    let defTypeParams g =
        opt (sepBy1 g.TypeParameter gComma |> gAngular)
        |>> function
            | None -> []
            | Some xs -> xs

    let gPredefinedType =
        let e k v = k >>. preturn v
        choice [
            e Lexer.Any S.Type.TAny
            e Lexer.Number S.Type.TNumber
            e Lexer.Boolean S.Type.TBoolean
            e Lexer.String S.Type.TString
            e Lexer.Void S.Type.TVoid
        ]

    let makeModuleName xs =
        List.fold (fun s t -> S.ModuleName.ModuleSubName(s, t))
            (S.ModuleName.ModuleId (List.head xs))
            (List.tail xs)

    let makeEntityName xs =
        match makeModuleName xs with
        | S.ModuleName.ModuleId root -> S.EntityName.ModuleEntity root
        | S.ModuleName.ModuleSubName (p, x) -> S.EntityName.InModuleEntity (p, x)

    let makeTypeName xs =
        match makeModuleName xs with
        | S.ModuleName.ModuleId root -> S.TypeName.TypeId root
        | S.ModuleName.ModuleSubName (p, x) -> S.TypeName.TypeSubName (p, x)

    let gModuleName =
        sepBy1 gIdent gDot
        |>> makeModuleName

    let gEntityName =
        sepBy1 gIdent gDot
        |>> makeEntityName

    let gTypeName =
        sepBy1 gIdent gDot
        |>> makeTypeName

    let defTypeArgs g =
        opt (sepBy1 g.Type gComma |> gAngular)
        |>> function
            | None -> []
            | Some xs -> xs

    let defTypeReference g : P<S.TypeReference> =
        pipe2 gTypeName g.TypeArgs <| fun tn args ->
            {
                TypeName = tn
                TypeArguments = args
            }

    let gTypeQuery =
        Lexer.TypeOf >>.
        pipe2 gIdent (many (gDot >>. gIdentName)) (fun id names ->
            List.fold
                (fun s t -> S.TQSub (s, t))
                (S.TQRoot id)
                names)

    let defTypeMember g : P<S.TypeMember> =
        choice [
            g.CallSignature |>> S.TypeMember.Call
            g.ConstructSignature |>> S.TypeMember.Construct
            g.IndexSignature |>> S.TypeMember.Index
            attempt g.MethodSignature
            g.PropertySignature |>> S.TypeMember.Property
        ]

    let defTypeBody g =
        sepEndBy g.TypeMember gSemi

    let defObjectType g =
        gCurly g.TypeBody

    let defFunctionType g =
        pipe3 g.TypeParams (g.Params .>> Lexer.``=>``) g.Type S.FunctionType

    let defConstructorType g =
        Lexer.New >>. pipe3 g.TypeParams (g.Params .>> Lexer.``=>``) g.Type S.ConstructorType

    let defType g =
        let t =
            gPredefinedType
            <|> (g.TypeReference |>> S.TReference)
            <|> (g.ObjectType |>> S.TObject)
        let t =
            pipe2 t (many (Lexer.``[`` .>> Lexer.``]``))
                (fun t xs -> List.fold (fun s () -> S.TArray s) t xs)
        t
        <|> (gTypeQuery |>> S.TQuery)
        <|> g.FunctionType
        <|> g.ConstructorType

    let defInterfaceDeclaration g : P<S.InterfaceDeclaration> =
        Lexer.Interface >>.
        pipe4
            gIdent
            g.TypeParams
            (opt (Lexer.Extends >>. sepBy1 g.TypeReference gComma))
            g.ObjectType
            (fun id tps ext ms ->
                {
                    Identifier = id
                    TypeParameters = tps
                    InterfaceExtends = defaultArg ext []
                    TypeMembers = ms
                })

    let gImportDeclaration : P<S.ImportDeclaration>=
        Lexer.Import >>.
        pipe2
            (gIdent .>> Lexer.Equal)
            (gEntityName .>> gSemi)
            (fun ident ent ->
                {
                    ImportIdentifier = ident
                    EntityName = ent
                })

    let gExternalImportDeclaration =
        pipe2
            (Lexer.Import >>. gIdent)
            (Lexer.Equal >>. Lexer.Require >>. gParen gStringLiteral)
            (fun id spec -> S.ImportRequire (id, spec))

    let defTypeAnnotation g =
        Lexer.``:`` >>. g.Type

    let defTypeAnnot g =
        opt (Lexer.``:`` >>. g.Type)
        |>> function
            | None -> S.TAny
            | Some t -> t

    let gOpt p v =
        opt p
        |>> fun x -> defaultArg x v

    type ParamShape =
        | POptional of Lexer.Id * S.Type
        | PRest of Lexer.Id * S.Type
        | PRequired of Lexer.Id * S.Type
        | PSpecialize of Lexer.Id * string

    let makeParams (sh: list<ParamShape>) : S.Parameters =
        let getRequired p =
            match p with
            | PRequired (id, t) -> Some (S.Param (id, t))
            | PSpecialize (id, s) -> Some (S.Specialize (id, s))
            | _ -> None
        let getOptional p =
            match p with
            | POptional (id, t) -> Some (S.Param (id, t))
            | _ -> None
        let getRest p =
            match p with
            | PRest (id, t) -> Some (S.Param (id, t))
            | _ -> None
        {
            Required = List.choose getRequired sh
            Optional = List.choose getOptional sh
            Rest = List.tryPick getRest sh
        }

    /// NOTE: public/private cases are skipped because declaration sources
    /// do not contain `ConstructorImplementation` (section 8.3.1).
    let defParams g =
        let param =
            let afterIdAndColon =
                (gStringLiteral |>> fun s id -> PSpecialize (id, s))
                <|> (g.Type |>> fun t id -> PRequired (id, t))
            let afterIdAndQ =
                g.TypeAnnot |>> fun t id -> POptional (id, t)
            let afterId =
                (Lexer.``:`` >>. afterIdAndColon)
                <|> (Lexer.``?`` >>. afterIdAndQ)
                <|> preturn (fun id -> PRequired (id, S.TAny))
            (pipe2 gIdent afterId (fun x f -> f x))
            <|> (Lexer.``...`` >>.
                    pipe2 gIdent g.TypeAnnot
                        (fun id ty -> PRest (id, ty)))
        sepBy param gComma
        |> gParen
        |>> makeParams

    let makeCallSignature gs ps ty : S.CallSignature =
        {
            TypeParameters = gs
            Parameters = ps
            ReturnType = ty
        }

    let defCallSignature g =
        pipe3 g.TypeParams g.Params g.TypeAnnot makeCallSignature

    let defConstructSignature g =
        Lexer.New >>. g.CallSignature

    let defIndexSignature g : P<S.IndexSignature> =
        let t =
            let e k v = k >>. preturn v
            choice [
                e Lexer.Number false
                e Lexer.String true
            ]
        pipe3
            (Lexer.``[`` >>. gIdent .>> Lexer.``:``)
            (t .>> Lexer.``]``)
            g.TypeAnnotation
            (fun id isString ty ->
                if isString then
                    S.IndexSignature.ByString(id, ty)
                else
                    S.IndexSignature.ByNumber(id, ty))

    let defPropertySignature g : P<S.PropertySignature> =
        pipe3 gPropertyName Lexer.Flags.``?`` g.TypeAnnot
            (fun name isOpt ty ->
                {
                    PropertyName = name
                    PropertyRequired = not isOpt
                    PropertyType = ty
                })

    let defMethodSignature g : P<S.TypeMember> =
        pipe3 gPropertyName Lexer.Flags.``?`` g.CallSignature
            (fun name isOpt cs -> S.Method (not isOpt) name cs)

    type ACBE = S.AmbientClassBodyElement

    let gAccess =
        opt (choice [Lexer.Public >>. preturn S.Public; Lexer.Private >>. preturn S.Private])
        |>> function
            | None -> S.Public
            | Some p -> p

    let gMemberScope =
        Lexer.Flags.Static
        |>> function
            | false -> S.Instance
            | true -> S.Static

    let defAmbientClassDeclaration g : P<S.AmbientClassDeclaration> =
        let ctor =
            Lexer.Constructor >>. g.Params .>> gSemi
            |>> ACBE.AmbientConstructor
        let mem =
            pipe4
                gAccess
                gMemberScope
                gPropertyName
                (gChoice2 g.TypeAnnot g.CallSignature)
                (fun acc sc name body ->
                    match body with
                    | Choice1Of2 ty ->
                        ACBE.AmbientProperty (acc, sc, name, ty)
                    | Choice2Of2 cs ->
                        ACBE.AmbientMethod (acc, sc, name, cs))
            .>> gSemi
        let heritage =
            fun x y -> (x, y)
            |> pipe2
                (opt (Lexer.Extends >>. g.TypeReference))
                (opt (Lexer.Implements >>. sepBy1 g.TypeReference gComma))
        pipe4
            (Lexer.Class >>. gIdent)
            g.TypeParams
            heritage
            (gCurly (many mem))
            (fun name tps (ext, impl) body ->
                {
                    ClassIdentifier = name
                    TypeParameters = tps
                    Extends = ext
                    Implements = defaultArg impl []
                    ClassBody = body
                })

    let gAmbientEnumDeclaration : P<S.AmbientEnumDeclaration> =
        let m =
            pipe2 gPropertyName (opt (Lexer.Equal >>. Lexer.IntegerLiteral)) <|
                fun id v ->
                    match v with
                    | None -> S.EnumComputed id
                    | Some v -> S.EnumConstant (id, v)
        fun id ms -> { EnumIdentifier = id; EnumMembers = ms } : S.AmbientEnumDeclaration
        |> pipe2
            (Lexer.Enum >>. gIdent)
            (gCurly (sepEndBy m gComma))

    let defDeclaration g : P<S.Declaration> =
        let varDecl =
            Lexer.Var
            >>. pipe2 gIdent g.TypeAnnot (fun id t ->
                    S.DeclareVar (id, t))
            .>> gSemi
        let funDecl =
            pipe2
                (Lexer.Function >>. gIdent)
                (g.CallSignature .>> gSemi)
                (fun id cs -> S.DeclareFunction (id, cs))
        varDecl
        <|> funDecl
        <|> (g.AmbientClassDeclaration |>> S.DeclareClass)
        <|> (gAmbientEnumDeclaration |>> S.DeclareEnum)

    let defModuleMember g : P<S.ModuleMember> =
        choice [
            g.Declaration |>> S.ModuleDeclare
            g.InterfaceDeclaration |>> S.ModuleInterface
            g.AmbientModuleDeclaration |>> S.ModuleNest
            g.ImportDeclaration |>> S.ModuleImport
        ]

    let makeAMD id es : S.AmbientModuleDeclaration =
        {
            ModuleId = id
            ModuleElements = es
        }

    let makeExportable exp el : S.Exportable<_> =
        {
            Element = el
            Export = exp
        }

    let defAmbientModuleDeclaration g : P<S.AmbientModuleDeclaration> =
        let p = pipe2 Lexer.Flags.Export g.ModuleMember makeExportable
        pipe2
            (Lexer.Module >>. gIdentifierPath)
            (gCurly (many p))
            (fun p es ->
                match List.rev p with
                | last :: inverted ->
                    List.fold (fun s t ->
                        makeAMD t [makeExportable true (S.ModuleNest s)])
                        (makeAMD last es)
                        inverted
                | _ ->
                    failwith "impossible")

    let gExportAssignment =
        Lexer.Export >>. Lexer.Equal >>. gIdent .>> gSemi
        |>> S.ExportAssignment.Export

    type AEME = S.AmbientExternalModuleElement

    let defAmbientExternalModuleDeclaration g : P<S.AmbientExternalModuleDeclaration> =
        let el : P<AEME> =
            attempt (gExportAssignment |>> AEME.Export)
            <|> pipe2
                Lexer.Flags.Export
                (gChoice2
                    g.ModuleMember
                    gExternalImportDeclaration)
                (fun exp mm ->
                    match mm with
                    | Choice1Of2 el ->
                        AEME.Member (makeExportable exp el)
                    | Choice2Of2 el ->
                        AEME.Import (makeExportable exp el))
        pipe2
            (Lexer.Module >>. gStringLiteral)
            (gCurly (many el))
            (fun p es ->
                {
                    ExternalModulePath = p
                    ExternalModuleElements = es
                })

    let defAmbientDeclaration g : P<S.AmbientDeclaration> =
        Lexer.Declare >>.
        choice [
            g.Declaration |>> S.Declare
            attempt g.AmbientExternalModuleDeclaration |>> S.DeclareExternalModule
            g.AmbientModuleDeclaration |>> S.DeclareModule
        ]

    let defDeclarationSourceFile g : P<S.DeclarationSourceFile> =
        let el =
            let v1 =
                gExportAssignment
                |>> S.DeclarationElement.Export
            let mk f x exp =
                let e = makeExportable exp x
                f e
            let def p f =
                p |>> mk f
            let v2 =
                pipe2
                    Lexer.Flags.Export
                    (choice [
                        def g.InterfaceDeclaration S.DeclarationElement.Interface
                        def gImportDeclaration S.DeclarationElement.Import
                        def gExternalImportDeclaration S.DeclarationElement.ExternalImport
                        def g.AmbientDeclaration S.DeclarationElement.Ambient
                    ])
                    (fun x f -> f x)
            v1 <|> v2
        many el
        |>> fun xs -> { DeclarationElements = xs }

    let grammar : Grammar =
        let rec mk (f: Grammar -> P<'T>) : P<'T> =
            let p = lazy f g
            parse.Delay(fun () -> p.Value)
        and g : Grammar =
            {
                AmbientClassDeclaration = mk defAmbientClassDeclaration
                AmbientDeclaration = mk defAmbientDeclaration
                AmbientExternalModuleDeclaration = mk defAmbientExternalModuleDeclaration
                AmbientModuleDeclaration = mk defAmbientModuleDeclaration
                CallSignature = mk defCallSignature
                ConstructSignature = mk defConstructSignature
                ConstructorType = mk defConstructorType
                Declaration = mk defDeclaration
                FunctionType = mk defFunctionType
                ImportDeclaration = gImportDeclaration
                IndexSignature = mk defIndexSignature
                InterfaceDeclaration = mk defInterfaceDeclaration
                MethodSignature = mk defMethodSignature
                ModuleMember = mk defModuleMember
                ObjectType = mk defObjectType
                Params = mk defParams
                PropertySignature = mk defPropertySignature
                Type = mk defType
                TypeAnnot = mk defTypeAnnot
                TypeAnnotation = mk defTypeAnnotation
                TypeBody = mk defTypeBody
                TypeArgs = mk defTypeArgs
                TypeMember = mk defTypeMember
                TypeParams = mk defTypeParams
                TypeParameter = mk defTypeParameter
                TypeReference = mk defTypeReference
            }
        g

    let DeclarationSourceFile : P<S.DeclarationSourceFile> =
        defDeclarationSourceFile grammar
        |> Lexer.Make
