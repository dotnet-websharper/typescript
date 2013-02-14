namespace Website

open IntelliFactory.Html
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Sitelets

type Action =
    | Home
    | About

module Client =

    type Facebook = IntelliFactory.TypeScript.Generator<file="C:\\Users\\toyvo\\home\\dev\\if-typescript\\IntelliFactory.TypeScript\\Test.d.ts">

    [<JavaScript>]
    let Main () =
        let opts = Facebook.InitOptions()
        opts.AppId <- "MyApp"
        opts.Cookie <- true
        Facebook.FB.Init()
        Facebook.FB.Init(opts)
        Facebook.FB.Api("A")
        Facebook.FB.Api("A", "B")
        Facebook.FB.Api("A", "B", 12)
        Facebook.FB.Api("A", "B", 12, fun x -> ())
        JavaScript.Log(opts)

module Controls =
    open IntelliFactory.WebSharper.Html

    [<Sealed>]
    type Main() =
        inherit Web.Control()

        [<JavaScript>]
        override this.Body =
            Client.Main()
            Div [] :> _


module Skin =
    open System.Web

    type Page =
        {
            Title : string
            Body : list<Content.HtmlElement>
        }

    let MainTemplate =
        let path = HttpContext.Current.Server.MapPath("~/Main.html")
        Content.Template<Page>(path)
            .With("title", fun x -> x.Title)
            .With("body", fun x -> x.Body)

    let WithTemplate title body : Content<Action> =
        Content.WithTemplate MainTemplate <| fun context ->
            {
                Title = title
                Body = body context
            }

module Site =

    let ( => ) text url =
        A [HRef url] -< [Text text]

    let Links (ctx: Context<Action>) =
        UL [
            LI ["Home" => ctx.Link Home]
            LI ["About" => ctx.Link About]
        ]

    let HomePage =
        Skin.WithTemplate "HomePage" <| fun ctx ->
            [
                Div [Text "HOME"]
                Div [new Controls.Main()]
                Links ctx
            ]

    let AboutPage =
        Skin.WithTemplate "AboutPage" <| fun ctx ->
            [
                Div [Text "ABOUT"]
                Links ctx
            ]

    let Main =
        Sitelet.Sum [
            Sitelet.Content "/" Home HomePage
            Sitelet.Content "/About" About AboutPage
        ]

type Website() =
    interface IWebsite<Action> with
        member this.Sitelet = Site.Main
        member this.Actions = [Home; About]

[<assembly: WebsiteAttribute(typeof<Website>)>]
do ()
