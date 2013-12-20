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

open System
open System.Reflection
open System.Reflection.Emit
open System.IO
open IntelliFactory.WebSharper.TypeScript

module C = Contracts
module R = ReflectEmit

//module internal ReflectEmitTest =
//
//    let container xs : R.CodeContainer =
//        { Nodes = xs }
//
//    let nB = Names.NameBuilder.Create()
//
//    let example =
//        let c1 =
//            let c1 = C.Contract()
//            c1.AddProp(nB.CreateName("Name"), C.TString)
//            c1
//        container [
//            "NumberProp", R.ValueNode C.TNumber
//            "StingProp", R.ValueNode C.TString
//            "C1Prop", R.ValueNode (C.TNamed (c1, []))
//            "F", R.ContainerNode <| container [
//                    "NumArrayProp", R.ValueNode (C.TArray C.TNumber)
//                    "C1", R.ContractNode c1
//                 ]
//        ]
//
//    let Run () =
//        let p = "Test.dll"
//        let b =
//            R.Construct {
//                AssemblyName = "A"
//                CodeContainer = example
//                TopLevelClassName = "A.Main"
//            }
//        File.WriteAllBytes(p, b)
//        printfn "Wrote %s" (Path.GetFullPath p)
