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

module A = Analysis
module C = Contracts

/// Name disambiguation pass: given the output of Analysis phase,
/// construct a trie that groups contracts and values into a module structure,
/// and assigns CLR-sensible identifiers to all named entities.
module internal Naming =

    [<Sealed>]
    type Id =
        member Text : string

    type Parameter<'T> = Shapes.Parameter<Id,'T>
    type Signature<'T> = Shapes.Signature<Id,'T>
    type Indexer<'T> = Shapes.Indexer<Id,'T>

    [<Sealed>]
    type Property<'T> =
        member Id : Id
        member Type : 'T

    [<ReferenceEquality>]
    [<NoComparison>]
    type Contract<'T> =
        {
            ByNumber : option<Indexer<'T>>
            ByString : option<Indexer<'T>>
            Call : seq<Signature<'T>>
            Extends : seq<'T>
            Generics : seq<Id>
            Name : Id
            New : seq<Signature<'T>>
            Properties : seq<Property<'T>>
        }

    type Type =
        | TAny
        | TArray of Type
        | TBoolean
        | TGeneric of int
        | TGenericM of int
        | TNamed of Contract<Type> * list<Type>
        | TNumber
        | TString

    [<Sealed>]
    type Value =
        member Id : Id
        member Type : Type

    type Contract = Contract<Type>
    type Property = Property<Type>
    type Signature = Signature<Type>

    [<Sealed>]
    type Module<'T> =
        member Id : 'T
        member Modules : seq<Module<Id>>
        member Contracts : seq<Contract>
        member Values : seq<Value>

    type NestedModule = Module<Id>
    type TopModule = Module<unit>

    val (|MethodType|_|) : Type -> option<seq<Signature>>

    val Do : A.Output -> TopModule
