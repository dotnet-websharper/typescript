/// Implements reduced TypeScript declarations grammar.
/// Performs type name resolution, handles scoping.
/// Also groups member overloads by name.
module internal IntelliFactory.TypeScript.Declarations.Reduced

module C = IntelliFactory.TypeScript.CSharp
module S = Syntax

[<Sealed>]
type Signature =
    member Parameters : S.Parameters
    member Return : S.Return

    static member Create : S.Parameters -> Signature
    static member Create : S.Parameters * S.Return -> Signature

type ClassMember =
    | ClassConstructor of seq<S.Parameters>
    | ClassMethod of C.MemberKind * S.Identifier * seq<Signature>
    | ClassProperty of C.MemberKind * S.Identifier * S.Type

type InterfaceMember =
    | InterfaceCall of seq<Signature>
    | InterfaceConstruct of seq<Signature>
    | InterfaceMethod of S.Identifier * seq<Signature>
    | InterfaceIndexer of seq<S.Identifier * S.Type * S.Type>
    | InterfaceProperty of S.Identifier * S.Type

type ModuleMember =
    | ModuleCall of seq<Signature>
    | ModuleMethod of S.Identifier * seq<Signature>
    | ModuleProperty of S.Identifier * S.Type

[<Sealed>]
type Class =
    member Implements : seq<S.Name>
    member Inherits : option<S.Name>
    member Members : seq<ClassMember>

[<Sealed>]
type Interface =
    member Extends : seq<S.Name>
    member Members : seq<InterfaceMember>

type Kind =
    | ClassKind of Class
    | EnumKind of seq<S.Identifier>
    | InterfaceKind of Interface

[<Sealed>]
type TypeDefinition =
    member Id : S.Identifier
    member Kind : Kind
    member Type : C.Type

[<Sealed>]
type Module =
    member Resolve : S.Name -> option<C.Type>
    member Members : seq<ModuleMember>
    member Modules : seq<S.Identifier * Module>
    member TypeDefinitions : seq<TypeDefinition>

[<Sealed>]
type DeclarationFile =
    member GlobalModule : Module

val CompileType : seq<S.TypeMember> -> seq<InterfaceMember>
val Compile : S.DeclarationSourceFile -> DeclarationFile
