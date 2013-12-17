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

/// Type shapes shared across mutliple passes.
module internal Shapes =

    type Parameter<'N,'T> =
        | Param of 'N * 'T
        | ParamConst of 'N * string

    type Signature<'N,'T> =
        {
            MethodGenerics : list<'N>
            Parameters : list<Parameter<'N,'T>>
            RestParameter : option<Parameter<'N,'T>>
            ReturnType : option<'T>
        }

    type Indexer<'N,'T> =
        {
            IndexerName : 'N
            IndexerType : 'T
        }
