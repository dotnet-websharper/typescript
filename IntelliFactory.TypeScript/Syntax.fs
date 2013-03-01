/// Defines abstract syntax types for the contents of
/// TypeScript declarations (.d.ts) files. Based on the 0.8
/// version of the TypeScript Language Spec, see:
/// http://www.typescriptlang.org/Content/TypeScript%20Language%20Specification.pdf
module internal IntelliFactory.TypeScript.Syntax

open System
open System.Collections.Generic
open System.Threading

type Identifier = Symbols.Syntax.Identifier
type Name = Symbols.Syntax.Name

type Requirement =
    | Optional
    | Required

(* See 3.5 *)

type Type =
    | AnyType
    | ArrayOf of Type
    | BoolType
    | NumberType
    | ObjectType of seq<TypeMember>
    | StringType
    | TypeName of Name

and TypeMember =
    | CallMember of CallSignature
    | ConstructMember of ConstructSignature
    | FunctionMember of FunctionSignature
    | IndexMember of IndexSignature
    | PropertyMember of PropertySignature

/// See 3.5.3.1
and CallSignature =
    | CallSignature of Parameters * Return

/// See 3.5.3.2
and ConstructSignature =
    | ConstructSignature of Parameters * Type

/// See 3.5.3.3
and IndexSignature =
    | IndexSignature of Parameter * Type

/// See 3.5.3.4
and PropertySignature =
    | PropertySignature of Identifier * Requirement * Type

(* See 3.5.3.5, 6.2 *)

and FunctionSignature =
    | FunctionSignature of Identifier * Requirement * Parameters * Return

and Parameter =
    | Parameter of Identifier * Type

and Parameters =
    {
        OptionalParameters : seq<Parameter>
        RequiredParameters : seq<Parameter>
        RestParameter : option<Parameter>
    }

and Return =
    | Void
    | Returns of Type

and Visibility =
    | Default
    | Private
    | Public

/// See 7.1
and InterfaceDeclaration =
    {
        ExtendedInterfaces : seq<Name>
        InterfaceMembers : seq<TypeMember>
        Name : Identifier
    }

/// See 9.2.2
and ImportDeclaration =
    | ExternalImport of Identifier * Path
    | InternalImport of Identifier * Name

and Path = string

(* See 10.1.3 *)

and AmbientClassDeclaration =
    {
        ClassMembers : seq<ClassMember>
        Implements : seq<Name>
        Inherits : option<Name>
        Name : Identifier
    }

and ClassMember =
    | Constructor of Parameters
    | InstanceFunction of Visibility * FunctionSignature
    | InstanceField of Visibility * Parameter
    | StaticField of Parameter
    | StaticFunction of FunctionSignature

(* See 10.1.4 *)

type EnumDeclaration =
    {
        Name : Identifier
        Variants : seq<Identifier>
    }

type InternalModule =
    {
        Name : Name
        Members : seq<ModuleElement>
    }

and ModuleElement =
    | CallElement of CallSignature
    | ClassElement of AmbientClassDeclaration
    | EnumElement of EnumDeclaration
    | FunctionElement of FunctionSignature
    | ImportElement of ImportDeclaration
    | InterfaceElement of InterfaceDeclaration
    | ModuleElement of InternalModule
    | VariableElement of Parameter

type ExternalModule =
    {
        Members : seq<ModuleElement>
        Path : string
    }

type Declaration =
    | ExternalModuleDeclaration of ExternalModule
    | RegularDeclaration of ModuleElement

/// See 10.2
type DeclarationSourceFile =
    | DeclarationSourceFile of seq<Declaration>
