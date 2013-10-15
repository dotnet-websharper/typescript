namespace IntelliFactory.WebSharper.TypeScript

/// Expresses a set of TypeScript code after analysis
/// (name resolution, etc) has been performed.
module CodeModel =

    /// Assume for now valid TS identifier, whatever that means.
    type Identifier = string

    type ExternalModulePath =
        | NestedPath of ExternalModulePath * Identifier
        | RootPath of Identifier

    type Module =
        | GlobalModule
        | ExternalModule of ExternalModulePath
        | NestedModule of Module * Identifier

    type TypeParameter =
        | MethodGeneric of Identifier * int // 0-based position among method generics.
        | TypeGeneric of Identifier * int // 0-based position among type generics.

    type Location =
        {
            Module : Module
            Name : Identifier
        }

    type ContractLocation =
        | Anonymous of Module // where we found this contract
        | Named of Location

    type PropertyName =
        | IntegerPropertyName of int
        | TextPropertyName of string

    type PropertySignature =
        {
            IsRequiredProperty : bool
            PropertyName : PropertyName
            PropertyType : Type
        }

    and Parameter =
        {
            ParameterName : Identifier
            ParameterType : Type
        }

    and Parameters =
        {
            Optional : list<Parameter>
            Required : list<Parameter>
            Rest : option<Parameter>
        }

    and CallSignature =
        {
            Parameters : Parameters
            ReturnType : Type

            /// Type parameters (generics).
            TypeParameters : list<TypeParameter>
        }

    and IndexSignature =
        | ByNumber of Type
        | ByString of Type

    /// Note: methods are special case of Property, can do Active pattern + method constructor.
    and TypeMember =
        | Call of CallSignature
        | Construct of CallSignature
        | Index of IndexSignature
        | Property of PropertySignature

    and Contract =
        {
            Location : ContractLocation

            /// Members - properties, indexers, etc.
            Members : list<TypeMember>

            /// Type parameters (generics).
            TypeParameters : list<TypeParameter>
        }

    and Instance =
        {
            Location : Location
            Type : Type
        }

    and Type =
        | Any
        | Array of Type
        | Boolean
        | ContractType of Contract
        | Generic of TypeParameter
        | Number
        | String
        | Void

    type CodeSet =
        {
             Contracts : seq<Contract>
             Instances : seq<Instance>
        }
