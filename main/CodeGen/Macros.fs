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

namespace WebSharper.TypeScript

module Macros =
    open WebSharper.Core
    open WebSharper.Core.AST

    [<Sealed>]
    type CallMacro() =
        inherit Macro()

        override this.TranslateCall(c) =
            match c.Arguments with
            | target :: args -> MacroOk <| Application(target, args, NonPure, None)  
            | _ -> MacroError "Invalid application of the CallMacro"

    [<Sealed>]
    type ItemMacro() =
        inherit Macro()

        override this.TranslateCall(c) =
            match c.Arguments with
            | [ target; arg ] -> MacroOk <| ItemGet(target, arg, Pure)  
            | [ target; arg; value ] -> MacroOk <| ItemSet(target, arg, value)  
            | _ -> MacroError "Invalid application of the ItemMacro"

    [<Sealed>]
    type NewMacro() =
        inherit Macro()

        override this.TranslateCall(c) =
            match c.Arguments with
            | target :: args -> MacroOk <| New(target, args)  
            | _ -> MacroError "Invalid application of the NewMacro"
