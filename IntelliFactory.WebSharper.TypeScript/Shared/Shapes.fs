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

//    let IsSimpleParameter p =
//        match p with
//        | Param _ -> true
//        | _ -> false
//
//    let GetParameterType p =
//        match p with
//        | Param (_, t) -> Some t
//        | _ -> None

//    type ContractKind<'T> =
//        | EmptyContract
//        | FunctionContract of list<'T> * option<'T>
//        | MethodContract
//        | ObjectContract


//    let IsFunctionSignature (si: Signature<_,_>) =
//        si.RestParameter.IsNone
//        && si.MethodGenerics.IsEmpty
//        && List.forall IsSimpleParameter si.Parameters


//    let (|FunctionSignature|_|) (si: Signature<_,_>) =
//        if IsFunctionSignature si then
//            Some (List.choose GetParameterType si.Parameters, si.ReturnType)
//        else None
