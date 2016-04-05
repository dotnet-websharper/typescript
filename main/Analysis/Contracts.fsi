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

module S = Shapes

/// Representation of contracts after analysis has completed.
module internal Contracts =

    type Indexer<'T> = S.Indexer<Name,'T>
    type Parameter<'T> = S.Parameter<Name,'T>
    type Signature<'T> = S.Signature<Name,'T>

    type Property<'T> =
        | OptProp of 'T
        | Prop of 'T

        member IsOptional : bool
        member Value : 'T

    [<Sealed>]
    type Contract<'T> =
        member AddByNumber : Name * 'T -> unit
        member AddByString : Name * 'T -> unit
        member AddCall : Signature<'T> -> unit
        member AddNew : Signature<'T> -> unit
        member AddOptProp : Name * 'T -> unit
        member AddProp : Name * 'T -> unit
        member Extend : 'T -> unit
        member SetGenerics : list<Name> -> unit
        member ByString : option<Indexer<'T>>
        member ByNumber : option<Indexer<'T>>
        member Call : seq<Signature<'T>>
        member Extends : seq<'T>
        member Generics : list<Name>
        member HintPath : NamePath // with get, set

        /// Whether other contracts extend this one.
        member IsExtended : bool with get, set
        member ClassExtends : option<'T> with get,set

        /// The contract describes a syntactic entity found in 
        /// an anonymous position (not a named position such as an
        /// (interface, class, module, enum body).
        member IsAnonymous : bool

        member New : seq<Signature<'T>>
        member Properties : seq<KeyValuePair<Name,Property<'T>>>

    and [<Sealed>] Contracts<'T> =
        new : unit -> Contracts<'T>
        member AnonymousContract : hintPath: NamePath -> Contract<'T>
        member NamedContract  : hintPath: NamePath -> Contract<'T>
        member All : seq<Contract<'T>>

    type Type =
        | TAny
        | TArray of Type
        | TBoolean
        | TCompiled of System.Type * list<Type>
        | TGeneric of int
        | TGenericM of int
        | TLazy of Lazy<Type>
        | TNamed of Contract<Type> * list<Type>
        | TNumber
        | TString

    type Contract = Contract<Type>
    type Contracts = Contracts<Type>
    type Indexer = Indexer<Type>
    type Parameter = Parameter<Type>
    type Property = Property<Type>
    type Signature = Signature<Type>

    val TryGetAnonymousPropertyContract : Contract -> Name -> option<Contract>
