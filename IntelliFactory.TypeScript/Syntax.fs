/// Defines abstract syntax types for the contents of
/// TypeScript declarations (.d.ts) files. Based on the 0.8
/// version of the TypeScript Language Spec, see:
/// http://www.typescriptlang.org/Content/TypeScript%20Language%20Specification.pdf
module IntelliFactory.TypeScript.Syntax

open System
open System.Collections.Generic
open System.Threading
module M = Memoization

[<Sealed>]
type Identifier private (text: string) =
    let hash = hash text
    static let table = M.Memoize (M.Options()) (fun x -> Identifier(x))
    override this.Equals(other: obj) = Object.ReferenceEquals(this, other)
    override this.GetHashCode() = hash
    override this.ToString() = text
    member this.Text = text
    static member Create(text) = table.[text]
    interface IComparable with
        member this.CompareTo(other: obj) =
            compare text (other :?> Identifier).Text

[<Sealed>]
type Name private (node: NameNode) =
    static let table = M.Memoize (M.Options()) (fun x -> Name(x))
    static let identity = HashIdentity.Reference
    let head = node.Head
    let text = string node
    override this.Equals(other: obj) = Object.ReferenceEquals(this, other)
    override this.GetHashCode() = identity.GetHashCode(this)
    member this.Local(id: Identifier) = table.[LocalName (this, id)]
    member this.ToList() = this.ToList []
    member private this.ToList(acc) =
        match node with
        | GlobalName x -> List.rev (x :: acc)
        | LocalName (name, x) -> name.ToList (x:: acc)
    override this.ToString() = text
    member this.Head = head
    member this.Node = node
    static member Global(id: Identifier) = table.[GlobalName id]

    interface IComparable with
        member this.CompareTo(other: obj) =
            compare node (other :?> Name).Node

and NameNode =
    private
    | GlobalName of Identifier
    | LocalName of Name * Identifier

    member this.Head =
        match this with
        | GlobalName x -> x
        | LocalName (x, _) -> x.Head

    override this.ToString() =
        match this with
        | GlobalName a -> string a
        | LocalName (a, b) -> String.Format("{0}.{1}", a, b)

let (|GlobalName|LocalName|) (name: Name) =
    match name.Node with
    | GlobalName x -> GlobalName x
    | LocalName (x, y) -> LocalName (x, y)

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
