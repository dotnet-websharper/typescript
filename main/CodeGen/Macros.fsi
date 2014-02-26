﻿// $begin{copyright}
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

module M = IntelliFactory.WebSharper.Core.Macros

/// Implements WebSharper-related macros for compilation support.
module internal Macros =

    [<Sealed>]
    type CallMacro =
        new : unit -> CallMacro
        interface M.IMacroDefinition

    [<Sealed>]
    type ItemMacro =
        new : unit -> ItemMacro
        interface M.IMacroDefinition

    [<Sealed>]
    type NewMacro =
        new : unit -> NewMacro
        interface M.IMacroDefinition

//    [<Sealed>]
//    type NewConfigObjectMacro =
//        new : unit -> NewConfigObjectMacro
//        interface M.IMacroDefinition