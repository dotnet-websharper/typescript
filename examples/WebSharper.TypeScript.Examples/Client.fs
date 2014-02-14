namespace WebSharper.TypeScript.Examples

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
type Epic = WebSharper.EpicEditor

[<JavaScript>]
module Client =

    let AddEditor (el: Html.Element) =

        // NOTE: need to allow constructing types with all-optional properties.
        let opts = New [] |> As<Epic.EpicEditorOptions>
        opts.container <- el.Dom
        opts.file <-
            let file = New [] |> As<Epic.Anon>
            file.name <- "My Document"
            file.defaultContent <- "# My Document"
            file.autoSave <- 100
            file
        opts.basePath <- "/EpicEditor-v0.2.2"

        // NOTE: binding taken from DefinitelyTyped is outdated wrt to EpicEditor binding.
        opts?theme <-
            New [
                "base" => "/themes/base/epiceditor.css"
                "preview" => "/themes/preview/preview-dark.css"
                "editor" => "/themes/editor/epic-dark.css"
            ]

        // NOTE: the way static and instance members are named for a class here,
        // it works but looks ugly (EpicEditor1).
        let ed = Epic.EpicEditor1.New(opts)

        ed.load()
        |> ignore

    let Main () =
        let container = Div []
        container
        |>! OnAfterRender (fun self ->
            AddEditor self)
