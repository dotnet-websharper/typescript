/// Defines abstract syntax types for the contents of
/// TypeScript declarations (.d.ts) files. Based on the 0.8
/// version of the TypeScript Language Spec, see:
/// http://www.typescriptlang.org/Content/TypeScript%20Language%20Specification.pdf
module internal IntelliFactory.TypeScript.Syntax

open System
open System.Collections.Generic
open System.Threading
module M = Memoization

type Identifier = Symbols.Symbol<Symbols.SyntaxChecker>
type Name = Symbols.Name<Symbols.SyntaxChecker>

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
    | FunctionCallSignature of Requirement * Parameters * Return
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
    | ExternalImport of Identifier * string
    | InternalImport of Identifier * Name

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
    | VariableElement of Parameter
    | FunctionElement of FunctionSignature
    | ClassElement of AmbientClassDeclaration
    | InterfaceElement of InterfaceDeclaration
    | ModuleElement of InternalModule
    | ImportElement of ImportDeclaration
    | EnumElement of EnumDeclaration

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
