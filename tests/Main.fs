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

namespace WebSharper.TypeScript.Tests

open WebSharper.Html.Server
open WebSharper
open WebSharper.Sitelets

type Action =
    | Main

module Controls =
    open WebSharper.Html.Client

    [<Sealed>]
    type EntryPoint() =
        inherit Web.Control()

        [<JavaScript>]
        override __.Body =
            ClientTests.Main()
            Div [] :> _

module Skin =
    open System.Web

    type Page =
        {
            Title : string
            Body : list<Content.HtmlElement>
        }

    let MainTemplate =
        Content.Template<Page>("~/Main.html")
            .With("title", fun x -> x.Title)
            .With("body", fun x -> x.Body)

    let WithTemplate title body : Content<Action> =
        Content.WithTemplate MainTemplate <| fun context ->
            {
                Title = title
                Body = body context
            }

module Site =

    let Main =
        let t = "WebSharper.TypeScript Tests"
        Skin.WithTemplate t (fun ctx ->
            [
                Div [new Controls.EntryPoint()]
            ])
        |> Sitelet.Content "/" Main

[<Sealed>]
type Website() =
    interface IWebsite<Action> with
        member this.Sitelet = Site.Main
        member this.Actions = [Main]

[<Require(typeof<JQuery.Resources.JQuery>)>]
[<Sealed>]
type QUnit() =
    inherit Resources.BaseResource("//code.jquery.com/qunit/",
        "qunit-1.13.0.js", "qunit-1.13.0.css")

[<assembly: Require(typeof<QUnit>)>]
[<assembly: Website(typeof<Website>)>]
do ()
