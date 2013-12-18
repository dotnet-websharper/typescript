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

namespace IntelliFactory.WebSharper.TypeScript

module S = Shapes

/// Representation of contracts after analysis has completed.
module internal Contracts =

    type Indexer<'T> = S.Indexer<Name,'T>
    type Parameter<'T> = S.Parameter<Name,'T>
    type Signature<'T> = S.Signature<Name,'T>

    type Property<'T> =
        | OptProp of 'T
        | Prop of 'T

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

        /// Sets IsUsedAsNamed to `true`.
        member MarkNamedUse : unit -> unit

        member SetGenerics : list<Name> -> unit

        member ByString : option<Indexer<'T>>
        member ByNumber : option<Indexer<'T>>
        member Call : seq<Signature<'T>>
        member Extends : seq<'T>
        member Generics : list<Name>
        member HintPath : NamePath with get, set

        /// Tracks if the contract has been used in a named position,
        /// such as a body of a named interface (class, etc), as a base type
        /// in extends clause, a method type inside an anonymous object type,
        /// or as a result of type query. Knowing this is necessary to decide
        /// which contracts with Kind = MethodContract or FunctionContract
        /// do not need to be represented as a reified CLR class.
        member IsUsedAsNamed : bool

        member Kind : S.ContractKind<'T>
        member New : seq<Signature<'T>>
        member Properties : IReadOnlyDictionary<Name,Property<'T>>

    and [<Sealed>] Contracts<'T> =
        new : unit -> Contracts<'T>
        member Contract : unit -> Contract<'T>
        member All : seq<Contract<'T>>

    type Type =
        | TAny
        | TArray of Type
        | TBoolean
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

