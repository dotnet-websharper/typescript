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

namespace WebSharper.TypeScript

#nowarn "40"

open FParsec
module E = ExternalModuleNames
module S = Syntax

module Parser =

    type internal UserState = Lexer.UserState
    type internal Parser<'T> = Parser<'T,Lexer.UserState>
    type P<'T> = Parser<'T>

    type Grammar =
        {
            AmbientClassDeclaration : P<S.AmbientClassDeclaration>
            AmbientDeclaration : P<S.AmbientDeclaration>
            AmbientExternalModuleDeclaration : P<S.AmbientExternalModuleDeclaration>
            AmbientModuleDeclaration : P<S.AmbientModuleDeclaration>
            AmbientModuleElement : P<S.AmbientModuleElement>
            CallSignature : P<S.CallSignature>
            ConstructSignature : P<S.CallSignature>
            ConstructorType : P<S.Type>
            FunctionType : P<S.Type>
            ImportDeclaration : P<S.ImportDeclaration>
            IndexSignature : P<S.IndexSignature>
            InterfaceDeclaration : P<S.InterfaceDeclaration>
            MethodSignature : P<S.TypeMember>
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
    let gPropertyName : P<Name> =
        pipe2
            getUserState
            (choice [
                Lexer.IdentifierName |>> fun x -> x.Text
                Lexer.StringLiteral
                Lexer.IntegerLiteral |>> string
            ])
            (fun us x -> us.IdBuilder.CreateName(x))

    let gIdentName = Lexer.IdentifierName
    let gIdent = Lexer.Identifier

    let gSemi = Lexer.ActualOrImpliedSemicolon

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
            match c with
            | None -> S.TP1 id
            | Some x -> S.TP2 (id, x)

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
        List.fold (fun s t -> S.MN2(s, t))
            (S.MN1 (List.head xs))
            (List.tail xs)

    let makeEntityName xs =
        match makeModuleName xs with
        | S.MN1 root -> S.EN1 root
        | S.MN2 (p, x) -> S.EN2 (p, x)

    let makeTypeName xs =
        match makeModuleName xs with
        | S.MN1 root -> S.TN1 root
        | S.MN2 (p, x) -> S.TN2 (p, x)

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
            S.TRef (tn, args)

    let gTypeQuery =
        Lexer.TypeOf >>.
        pipe2 gIdent (many (gDot >>. gIdentName)) (fun id names ->
            List.fold
                (fun s t -> S.TQ2 (s, t))
                (S.TQ1 id)
                names)

    let defTypeMember g : P<S.TypeMember> =
        choice [
            g.CallSignature |>> S.TM2
            g.IndexSignature |>> S.TM4
            attempt (g.ConstructSignature |>> S.TM3)
            attempt g.MethodSignature
            g.PropertySignature |>> S.TM1
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
                    InterfaceName = id
                    InterfaceTypeParameters = tps
                    InterfaceExtends =
                        match ext with
                        | None -> []
                        | Some xs -> xs
                    InterfaceBody = ms
                })

    let gImportDeclaration : P<S.ImportDeclaration>=
        Lexer.Import >>.
        pipe2
            (gIdent .>> Lexer.Equal)
            (gEntityName .>> gSemi)
            (fun ident ent -> S.ID (ident, ent))

    let gExternalImportDeclaration =
        pipe3
            getUserState
            (Lexer.Import >>. gIdent)
            (Lexer.Equal >>. Lexer.Require >>. gParen gStringLiteral)
            (fun st id spec ->
                let name = ExternalModuleNames.Name.Parse(st.IdBuilder, spec)
                S.EID (id, name))

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
            | PRequired (id, t) -> Some (S.P1 (id, t))
            | PSpecialize (id, s) -> Some (S.P2 (id, s))
            | _ -> None
        let getOptional p =
            match p with
            | POptional (id, t) -> Some (S.P1 (id, t))
            | _ -> None
        let getRest p =
            match p with
            | PRest (id, t) -> Some (S.P1 (id, t))
            | _ -> None
        let req = List.choose getRequired sh
        let opt = List.choose getOptional sh
        let rest = List.tryPick getRest sh
        match rest with
        | Some r -> S.Ps3 (req, opt, r)
        | None ->
            match opt with
            | [] -> S.Ps1 req
            | opt -> S.Ps2 (req, opt)

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
        S.CS (gs, ps, ty)

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
                if isOpt then
                    S.OptProp (name, ty)
                else
                    S.Prop (name, ty))

    let defMethodSignature g : P<S.TypeMember> =
        pipe3 gPropertyName Lexer.Flags.``?`` g.CallSignature
            (fun name isOpt cs ->
                if isOpt then
                    S.OptionalMethod name cs
                else
                    S.RequiredMethod name cs)

    type internal ACBE = S.AmbientClassBodyElement

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
            gAccess >>. Lexer.Constructor >>. g.Params .>> gSemi
            |>> ACBE.ClassConstructor
        let mem =
            (
                choice [
                    attempt ctor 
                    pipe4
                        gAccess
                        gMemberScope
                        gPropertyName
                        (gChoice2 g.CallSignature g.TypeAnnot)
                        (fun acc sc name body ->
                            match body with
                            | Choice2Of2 ty ->
                                ACBE.ClassProperty (acc, sc, name, ty)
                            | Choice1Of2 cs ->
                                ACBE.ClassMethod (acc, sc, name, cs))
                        .>> gSemi
                    g.IndexSignature |>> ACBE.ClassIndex .>> gSemi
                ]
            )
            <?> "member"
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
                    ClassName = name
                    ClassTypeParameters = tps
                    ClassExtends =  ext
                    ClassImplements = defaultArg impl []
                    ClassBody = body
                })

    let gAmbientEnumDeclaration : P<S.AmbientEnumDeclaration> =
        let m =
            pipe2 gPropertyName (opt (Lexer.Equal >>. Lexer.IntegerLiteral)) <|
                fun id v ->
                    match v with
                    | None -> S.AEM1 id
                    | Some v -> S.AEM2 (id, v)
        fun id ms -> { EnumName = id; EnumBody = ms } : S.AmbientEnumDeclaration
        |> pipe2
            (Lexer.Enum >>. gIdent)
            (gCurly (sepEndBy m gComma))

    let defVarDecl g =
        Lexer.Var
        >>. pipe2 gIdent g.TypeAnnot (fun id t -> S.AVD (id, t))
        .>> gSemi

    let defFunDecl g =
        pipe2
            (Lexer.Function >>. gIdent)
            (g.CallSignature .>> gSemi)
            (fun id cs -> S.AFD (id, cs))

    let defAmbientModuleElement g : P<S.AmbientModuleElement> =
        pipe2
            Lexer.Flags.Export
            (choice
                [
                    defVarDecl g |>> S.AME1
                    defFunDecl g |>> S.AME2
                    g.AmbientClassDeclaration |>> S.AME3
                    g.InterfaceDeclaration |>> S.AME4
                    gAmbientEnumDeclaration |>> S.AME5
                    g.AmbientModuleDeclaration |>> S.AME6
                    g.ImportDeclaration |>> fun x -> S.AME7 (S.NoExport, x)
                ])
            (fun x y ->
                match x, y with
                | true, S.AME7 (_, x) -> S.AME7 (S.Export, x)
                | _ -> y)

    let defAmbientModuleDeclaration g : P<S.AmbientModuleDeclaration> =
        pipe2
            (Lexer.Module >>. gIdentifierPath)
            (gCurly (many g.AmbientModuleElement))
            (fun p es -> S.AmbientModuleDeclaration.Create(p, es))

    let gExportAssignment =
        Lexer.Export >>. Lexer.Equal >>. gIdent .>> gSemi |>> S.EA

    let getTopLevelName name =
        match name with
        | E.TopLevel n -> n
        | E.Relative name ->
            failwithf "Relative names not allowed: %s" name.Text

    let defAmbientExternalModuleDeclaration g : P<S.AmbientExternalModuleDeclaration> =
        let el : P<S.AmbientExternalModuleElement> =
            attempt (gExportAssignment |>> S.AEME2)
            <|> pipe2
                Lexer.Flags.Export
                (gChoice2
                    g.AmbientModuleElement
                    gExternalImportDeclaration)
                (fun exp mm ->
                    match mm with
                    | Choice1Of2 el ->
                        S.AEME1 el
                    | Choice2Of2 el ->
                        let exp = if exp then S.Export else S.NoExport
                        let el =
                            match el with
                            | S.EID (id, name) ->
                                S.EID (id, getTopLevelName name)
                        S.AEME3 (exp, el))
        pipe3
            getUserState
            (Lexer.Module >>. gStringLiteral)
            (gCurly (many el))
            (fun us p es ->
                let name = E.Name.Parse(us.IdBuilder, p)
                S.AEMD (getTopLevelName name, es))

    let defAmbientDeclaration g : P<S.AmbientDeclaration> =
        Lexer.Declare >>.
        choice [
            defVarDecl g |>> S.AD1
            defFunDecl g |>> S.AD2
            g.AmbientClassDeclaration |>> S.AD3
            gAmbientEnumDeclaration |>> S.AD4
            attempt g.AmbientExternalModuleDeclaration |>> S.AD6
            g.AmbientModuleDeclaration |>> S.AD5
        ]

    let gChoice4 a b c d =
        choice [
            a |>> Choice1Of4
            b |>> Choice2Of4
            c |>> Choice3Of4
            d |>> Choice4Of4
        ]

    let defDeclarationSourceFile g : P<S.DeclarationSourceFile> =
        let el =
            let v1 = gExportAssignment |>> S.DE1
            let v2 =
                pipe2
                    (Lexer.Flags.Export
                        |>> function
                            | true -> S.Export
                            | false -> S.NoExport)
                    (gChoice4
                        g.InterfaceDeclaration
                        gImportDeclaration
                        gExternalImportDeclaration
                        g.AmbientDeclaration)
                    (fun x y ->
                        match y with
                        | Choice1Of4 y -> S.DE2 (x, y)
                        | Choice2Of4 y -> S.DE3 (x, y)
                        | Choice3Of4 y -> S.DE4 (x, y)
                        | Choice4Of4 y -> S.DE5 (x, y))
            v1 <|> v2
        many el
        |>> S.DSF

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
                AmbientModuleElement = mk defAmbientModuleElement
                CallSignature = mk defCallSignature
                ConstructSignature = mk defConstructSignature
                ConstructorType = mk defConstructorType
                FunctionType = mk defFunctionType
                ImportDeclaration = gImportDeclaration
                IndexSignature = mk defIndexSignature
                InterfaceDeclaration = mk defInterfaceDeclaration
                MethodSignature = mk defMethodSignature
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

    type Result =
        | ParseFailed of string
        | ParseOk of S.DeclarationSourceFile * seq<string>

    let ParseFile nameBuilder file =
        let uS = UserState.Create nameBuilder
        let result =
            runParserOnFile DeclarationSourceFile uS file Encoding.Default
        match result with
        | Success (res, state, pos) -> ParseOk (res, state.ReferencePaths)
        | Failure (error, _, _) -> ParseFailed error
