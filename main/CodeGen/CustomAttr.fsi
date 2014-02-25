// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

/// Constructs custom attribute values for WebSharper annotations.
module internal CustomAttr =
    open System

    val Call : CustomAttributeBuilder
    val ConfigObjectCtor : CustomAttributeBuilder
    val Item : CustomAttributeBuilder
    val Method : string -> arity: int -> CustomAttributeBuilder
    val MethodWithParamArray : string -> arity: int -> CustomAttributeBuilder
    val New : CustomAttributeBuilder
    val ParamArray : CustomAttributeBuilder
    val PropertyGet : string -> CustomAttributeBuilder
    val PropertySet : string -> CustomAttributeBuilder
    val Constructor : NamePath -> arity: int -> CustomAttributeBuilder
    val StaticMethod : NamePath -> arity: int -> CustomAttributeBuilder
    val StaticMethodWithParamArray : NamePath -> arity: int -> CustomAttributeBuilder
    val StaticPropertyGet : NamePath -> CustomAttributeBuilder
    val StaticPropertySet : NamePath -> CustomAttributeBuilder

