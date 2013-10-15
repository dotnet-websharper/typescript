namespace IntelliFactory.WebSharper.TypeScript

/// Defines the syntax tree for the declarations (`.d.ts`)
/// subset of TypeScript, extracted by tracing the grammar
/// from `DeclarationSourceFile`.
module Syntax =

    type Identifier = Lexer.Id

    [<Sealed>]
    type PropertyName =
        static member Create : string -> PropertyName

    type ModuleName =
        | ModuleId of Identifier
        | ModuleSubName of ModuleName * Identifier

    type TypeName =
        | TypeId of Identifier
        | TypeSubName of ModuleName * Identifier

    type TypeParameter<'T> =
        {
            Identifier : Identifier
            Extends : option<'T>
        }

    type TypeReference<'T> =
        {
            TypeName : TypeName
            TypeArguments : list<'T>
        }

    type Parameter<'T> =
        | Param of Identifier * 'T
        | Specialize of Identifier * string

    type Parameters<'T> =
        {
            Required : list<Parameter<'T>>
            Optional : list<Parameter<'T>>
            Rest : option<Parameter<'T>>
        }

    type PropertySignature<'T> =
        {
            PropertyName : PropertyName
            PropertyRequired : bool
            PropertyType : 'T
        }

    type CallSignature<'T> =
        {
            TypeParameters : list<TypeParameter<'T>>
            Parameters : Parameters<'T>
            ReturnType : 'T
        }

    type IndexSignature<'T> =
        | ByNumber of Identifier * 'T
        | ByString of Identifier * 'T

    type TypeMember<'T> =
        | Property of PropertySignature<'T>
        | Call of CallSignature<'T>
        | Construct of CallSignature<'T>
        | Index of IndexSignature<'T>

    type TypeQuery =
        | TQRoot of Identifier
        | TQSub of TypeQuery * Names.Name

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

    val FunctionType :
        typeParameters: list<TypeParameter<Type>> ->
        parameters: Parameters<Type> ->
        returnType: Type ->
        Type

    val ConstructorType :
        typeParameters: list<TypeParameter<Type>> ->
        parameters: Parameters<Type> ->
        returnType: Type ->
        Type

    val Method :
        isRequired: bool ->
        name: PropertyName ->
        callSignature: CallSignature ->
        TypeMember

    type InterfaceDeclaration =
        {
            Identifier : Identifier
            InterfaceExtends : list<TypeReference>
            TypeParameters : list<TypeParameter>
            TypeMembers : list<TypeMember>
        }

    type ExportAssignment =
        | Export of Identifier

    type Access =
        | Public
        | Private

    type MemberScope =
        | Instance
        | Static

    type AmbientClassBodyElement =
        | AmbientConstructor of Parameters
        | AmbientProperty of Access * MemberScope * PropertyName * Type
        | AmbientMethod of Access * MemberScope * PropertyName * CallSignature
        | AmbientIndex of IndexSignature

    type AmbientClassDeclaration =
        {
            ClassIdentifier : Identifier
            TypeParameters : list<TypeParameter>
            Extends : option<TypeReference>
            Implements : list<TypeReference>
            ClassBody : list<AmbientClassBodyElement>
        }

    type EnumMember =
        | EnumComputed of PropertyName
        | EnumConstant of PropertyName * int

    type AmbientEnumDeclaration =
        {
            EnumIdentifier : Identifier
            EnumMembers : list<EnumMember>
        }

    type Declaration =
        | DeclareVar of Identifier * Type
        | DeclareFunction of Identifier * CallSignature
        | DeclareClass of AmbientClassDeclaration
        | DeclareEnum of AmbientEnumDeclaration

    type Exportable<'T> =
        {
            Export : bool
            Element : 'T
        }

    type AmbientModuleDeclaration<'T> =
        {
            ModuleId : Identifier
            ModuleElements : list<Exportable<'T>>
        }

    type EntityName =
        | ModuleEntity of Identifier
        | InModuleEntity of ModuleName * Identifier

    type ImportDeclaration =
        {
            ImportIdentifier : Identifier
            EntityName : EntityName
        }

    type ModuleMember =
        | ModuleDeclare of Declaration
        | ModuleInterface of InterfaceDeclaration
        | ModuleNest of AmbientModuleDeclaration<ModuleMember>
        | ModuleImport of ImportDeclaration

    type AmbientModuleDeclaration =
        AmbientModuleDeclaration<ModuleMember>

    type ExternalImportDeclaration =
        | ImportRequire of Identifier * string

    type AmbientExternalModuleElement =
        | Member of Exportable<ModuleMember>
        | Export of ExportAssignment
        | Import of Exportable<ExternalImportDeclaration>

    type AmbientExternalModuleDeclaration =
        {
            ExternalModulePath : string
            ExternalModuleElements : list<AmbientExternalModuleElement>
        }

    type AmbientDeclaration =
        | Declare of Declaration
        | DeclareModule of AmbientModuleDeclaration
        | DeclareExternalModule of AmbientExternalModuleDeclaration

    type DeclarationElement =
        | Export of ExportAssignment
        | Interface of Exportable<InterfaceDeclaration>
        | Import of Exportable<ImportDeclaration>
        | ExternalImport of Exportable<ExternalImportDeclaration>
        | Ambient of Exportable<AmbientDeclaration>

    type DeclarationSourceFile =
        {
            DeclarationElements : list<DeclarationElement>
        }
