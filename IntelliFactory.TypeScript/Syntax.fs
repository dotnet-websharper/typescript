/// Defines abstract syntax types for the contents of
/// TypeScript declarations (.d.ts) files. Based on the 0.8
/// version of the TypeScript Language Spec, see:
/// http://www.typescriptlang.org/Content/TypeScript%20Language%20Specification.pdf
module internal IntelliFactory.TypeScript.Declarations.Syntax

type Identifier =
    | Identifier of string

    override this.ToString() =
        match this with
        | Identifier x -> x

type Name =
    {
        Name : Identifier
        Namespace : list<Identifier>
    }

    member this.Item
        with get (x: Identifier) =
            {
                Name = x
                Namespace =
                    [
                        yield! this.Namespace
                        yield this.Name
                    ]
            }

    member this.Item
        with get (x: Name) =
            {
                Name = x.Name
                Namespace =
                    [
                        yield! this.Namespace
                        yield this.Name
                        yield! x.Namespace
                    ]
            }

    static member Global(id: Identifier) =
        {
            Name = id
            Namespace = []
        }

    override this.ToString() =
        Seq.append this.Namespace (Seq.singleton this.Name)
        |> Seq.map string
        |> String.concat "."

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
    | EnumDeclaration of Identifier * seq<Identifier>

    member this.Name =
        match this with
        | EnumDeclaration (id, _) -> id

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
