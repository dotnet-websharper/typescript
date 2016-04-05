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

module E = ExternalModuleNames

module Syntax =

    (* Identifiers, names and modifiers *)

    type Identifier = Name

    type ModuleName =
        | MN1 of Identifier
        | MN2 of ModuleName * Identifier

        member x.ToName() =
            match x with
            | MN1 x -> Names.NP1 x
            | MN2 (x, y) -> Names.NP2 (x.ToName(), y)

        override x.ToString() =
            x.ToName() |> string

    type TypeName =
        | TN1 of Identifier
        | TN2 of ModuleName * Identifier

        member x.ToName() =
            match x with
            | TN1 x -> Names.NP1 x
            | TN2 (x, y) -> Names.NP2 (x.ToName(), y)

        override x.ToString() =
            x.ToName() |> string

    type EntityName =
        | EN1 of Identifier
        | EN2 of ModuleName * Identifier

        member x.ToName() =
            match x with
            | EN1 x -> Names.NP1 x
            | EN2 (x, y) -> Names.NP2 (x.ToName(), y)

        override x.ToString() =
            x.ToName() |> string

    type Access =
        | Private
        | Public

    type MemberScope =
        | Instance
        | Static

    type ExportModifier =
        | Export
        | NoExport

    (* Types *)

    type TypeParameter<'T> =
        | TP1 of Identifier
        | TP2 of Identifier * 'T

    type TypeReference<'T> =
        | TRef of TypeName * list<'T>

    type Parameter<'T> =
        | P1 of Identifier * 'T
        | P2 of Identifier * string

    type Parameters<'T> =
        | Ps1 of list<Parameter<'T>>
        | Ps2 of list<Parameter<'T>> * list<Parameter<'T>>
        | Ps3 of list<Parameter<'T>> * list<Parameter<'T>> * Parameter<'T>

    type PropertySignature<'T> =
        | OptProp of Name * 'T
        | Prop of Name * 'T

    type CallSignature<'T> =
        | CS of list<TypeParameter<'T>> * Parameters<'T> * 'T

    type ConstructSignature<'T> =
        CallSignature<'T>

    type IndexSignature<'T> =
        | ByNumber of Identifier * 'T
        | ByString of Identifier * 'T

    type TypeMember<'T> =
        | TM1 of PropertySignature<'T>
        | TM2 of CallSignature<'T>
        | TM3 of ConstructSignature<'T>
        | TM4 of IndexSignature<'T>

    type TypeQuery =
        | TQ1 of Identifier
        | TQ2 of TypeQuery * Names.Name

    type Type =
        | TAny
        | TNumber
        | TBoolean
        | TString
        | TVoid
        | TReference of TypeReference<Type>
        | TQuery of TypeQuery
        | TArray of Type
        | TObject of list<TypeMember<Type>>

    type CallSignature = CallSignature<Type>
    type IndexSignature = IndexSignature<Type>
    type TypeParameter = TypeParameter<Type>
    type TypeReference = TypeReference<Type>
    type TypeMember = TypeMember<Type>
    type Parameters = Parameters<Type>
    type PropertySignature = PropertySignature<Type>

    let defMethod isRequired name callSig =
        let ty = TObject [TM2 callSig]
        if isRequired then
            Prop (name, ty)
        else
            OptProp (name, ty)

    let OptionalMethod name callSig : TypeMember =
        TM1 (defMethod false name callSig)

    let RequiredMethod name callSig : TypeMember =
        TM1 (defMethod true name callSig)

    let defCallSignature tP ps rt =
        CS (tP, ps, rt)

    let FunctionType tP ps rt =
        TObject [TM2 (defCallSignature tP ps rt)]

    let ConstructorType tP ps rt =
        TObject [TM3 (defCallSignature tP ps rt)]

    (* Nodes *)

    type InterfaceDeclaration =
        {
            InterfaceName : Identifier
            InterfaceTypeParameters : list<TypeParameter>
            InterfaceExtends : list<TypeReference>
            ClassExtends : option<TypeReference>
            InterfaceBody : list<TypeMember>
        }

    type AmbientClassBodyElement =
        | ClassConstructor of Parameters
        | ClassProperty of Access * MemberScope * Name * Type
        | ClassMethod of Access * MemberScope * Name * CallSignature
        | ClassIndex of IndexSignature

    type AmbientClassDeclaration =
        {
            ClassName : Identifier
            ClassTypeParameters : list<TypeParameter>
            ClassExtends : option<TypeReference>
            ClassImplements : list<TypeReference>
            ClassBody : list<AmbientClassBodyElement>
        }

    type AmbientEnumMember =
        | AEM1 of Name
        | AEM2 of Name * int

    type AmbientEnumDeclaration =
        {
            EnumName : Identifier
            EnumBody : list<AmbientEnumMember>
        }

    type AmbientVariableDeclaration =
        | AVD of Identifier * Type

    type AmbientFunctionDeclaration =
        | AFD of Identifier * CallSignature

    type ImportDeclaration =
        | ID of Identifier * EntityName

    type ExportAssignment =
        | EA of Identifier

    type ExternalImportDeclaration<'T> =
        | EID of Identifier * 'T

    type AmbientModuleElement =
        | AME1 of AmbientVariableDeclaration
        | AME2 of AmbientFunctionDeclaration
        | AME3 of AmbientClassDeclaration
        | AME4 of InterfaceDeclaration
        | AME5 of AmbientEnumDeclaration
        | AME6 of AmbientModuleDeclaration
        | AME7 of ExportModifier * ImportDeclaration

    and AmbientModuleDeclaration =
        | AMD of Identifier * list<AmbientModuleElement>

        static member Create(ids, body) =
            match List.rev ids with
            | last :: inverted ->
                List.fold (fun s t -> AMD (t, [AME6 s]))
                    (AMD (last, (Seq.toList body)))
                    inverted
            | _ ->
                invalidArg "ids" "Identifier path cannot be empty"

    type AmbientExternalModuleElement =
        | AEME1 of AmbientModuleElement
        | AEME2 of ExportAssignment
        | AEME3 of ExportModifier * ExternalImportDeclaration<E.TopLevelName>

    type AmbientExternalModuleDeclaration =
        | AEMD of E.TopLevelName * list<AmbientExternalModuleElement>

    type AmbientDeclaration =
        | AD1 of AmbientVariableDeclaration
        | AD2 of AmbientFunctionDeclaration
        | AD3 of AmbientClassDeclaration
        | AD4 of AmbientEnumDeclaration
        | AD5 of AmbientModuleDeclaration
        | AD6 of AmbientExternalModuleDeclaration

    type DeclarationElement =
        | DE1 of ExportAssignment
        | DE2 of ExportModifier * InterfaceDeclaration
        | DE3 of ExportModifier * ImportDeclaration
        | DE4 of ExportModifier * ExternalImportDeclaration<E.Name>
        | DE5 of ExportModifier * AmbientDeclaration

    type DeclarationSourceFile =
        | DSF of list<DeclarationElement>
