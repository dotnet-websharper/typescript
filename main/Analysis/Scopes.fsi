﻿// $begin{copyright}
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

module C = Contracts
module S = Syntax

/// Provides explicit representations for modules, scopes, scope chains.
/// Implements the complicated name resolution rules.
module internal Scopes =

    type Contract =
        | Foreign of Type
        | Local of C.Contract

    [<Sealed>]
    type Module =
        new : C.Contracts * option<NamePath> -> Module
        member InternalRoot : Root
        member ExportedContracts : NameTable<Contract>
        member ExportedModules : NameTable<Module>
        member ExportedValues : NameTable<C.Type>

    and [<Sealed>] Root =
        member GetOrCreateModule : NamePath -> Module
        member GetOrCreateNamedContract : NamePath * ?suffix: Name -> Contract
        member GetOrCreateScope : NamePath -> Scope
        member IsGlobal : bool

    and [<Sealed>] Scope =
        new : unit -> Scope

        member BindContract : S.Identifier * Contract -> unit
        member BindModule : S.Identifier * Module -> unit
        member Link : S.Identifier * S.EntityName -> unit

    [<Sealed>]
    type ScopeChain =
        new : Logger -> ScopeChain
        member Add : Scope -> ScopeChain
        member ResolveModule : S.ModuleName -> option<Module>
        member ResolveContract : S.TypeName -> option<Contract>
        member ResolveType : S.TypeName * list<C.Type> -> C.Type

