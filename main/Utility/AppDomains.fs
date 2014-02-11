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

module AppDomains =

    type ITransform<'T1,'T2> =
        abstract Do : 'T1 -> 'T2
        abstract T1 : Pickler.T<'T1>
        abstract T2 : Pickler.T<'T2>

    type TypeMarker<'T> =
        | TypeMarker

    let MarkType<'T> : TypeMarker<'T> =
        TypeMarker

    type OpaqueJob =
        {
            Input : byte []
            T : Type
            T1 : Type
            T2 : Type
        }

    let OpaqueJobPickler =
        let inline ( ^ ) f x = f x
        Pickler.DefProduct (fun i t t1 t2 -> { Input = i; T = t; T1 = t1; T2 = t2 })
        ^ Pickler.Field (fun j -> j.Input) Pickler.Bytes
        ^ Pickler.Field (fun j -> j.T) Pickler.Type
        ^ Pickler.Field (fun j -> j.T1) Pickler.Type
        ^ Pickler.Field (fun j -> j.T2) Pickler.Type
        ^ Pickler.EndProduct ()

    type ITransformer =
        abstract Do : byte[] -> byte[]

    [<Sealed>]
    type OpaqueTransformer<'T,'T1,'T2>() =
        interface ITransformer with
            member x.Do(input: byte[]) : byte[] =
                let tr = Activator.CreateInstance(typeof<'T>) :?> ITransform<'T1,'T2>
                input
                |> Pickler.Unpickle tr.T1
                |> tr.Do
                |> Pickler.Pickle tr.T2

    let OpaqueTransform jobBytes =
        let job = Pickler.Unpickle OpaqueJobPickler jobBytes
        let ty = typedefof<OpaqueTransformer<_,_,_>>.MakeGenericType(job.T, job.T1, job.T2)
        let tr = Activator.CreateInstance(ty) :?> ITransformer
        tr.Do(job.Input)

    let SetupRedirects () =
        AppDomain.CurrentDomain.add_AssemblyResolve(fun h ev ->
            let name = AssemblyName(ev.Name)
            if name.Name = "FSharp.Core" then
                typedefof<list<_>>.Assembly
            else
                null)

    let CreateAppDomain () =
        let setup = AppDomainSetup()
        setup.ApplicationBase <-
            Assembly.GetExecutingAssembly().Location
            |> Path.GetDirectoryName
        let id = "Slave" + Guid.NewGuid().ToString().GetHashCode().ToString("x")
        AppDomain.CreateDomain(id, null, setup)

    [<Literal>]
    let Input = "Input"

    [<Literal>]
    let Output = "Output"

    let ShieldedLogic () =
        SetupRedirects ()
        let jobBytes = AppDomain.CurrentDomain.GetData(Input) :?> byte []
        let output = OpaqueTransform jobBytes
        AppDomain.CurrentDomain.SetData(Output, output)

    let TransformWithAppDomain<'A,'B,'T when 'T :> ITransform<'A,'B>
                                         and 'T : (new : unit -> 'T)>
        (marker: TypeMarker<'T>) (input: 'A) : 'B =
            let dom = CreateAppDomain ()
            try
                let inst = new 'T() :> ITransform<'A,'B>
                let jobBytes =
                    Pickler.Pickle OpaqueJobPickler {
                        Input = Pickler.Pickle inst.T1 input
                        T = typeof<'T>
                        T1 = typeof<'A>
                        T2 = typeof<'B>
                    }
                dom.SetData(Input, jobBytes)
                dom.DoCallBack(fun () -> ShieldedLogic ())
                dom.GetData(Output) :?> byte []
                |> Pickler.Unpickle inst.T2
            finally
                AppDomain.Unload(dom)


