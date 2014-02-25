namespace WebSharper.TypeScript.Examples

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
type Epic = WebSharper.EpicEditor

[<JavaScript>]
module Client =

    let AddEditor (el: Html.Element) =

        let opts =
            Epic.EpicEditorOptions(
                container = el.Dom,
                file =
                    (
                        /// NOTE: still no type-safe constructors for record-like objects.
                        let file = New [] |> As<Epic.Anon>
                        file.name <- "My Document"
                        file.defaultContent <- "# My Document"
                        file.autoSave <- 100
                        file
                    ),
                basePath =  "/EpicEditor-v0.2.2"
            )

        opts.theme <-
            let theme = New [] |> As<Epic.Anon1>
            theme._base <- "/themes/base/epiceditor.css"
            theme.preview <- "/themes/preview/preview-dark.css"
            theme.editor <- "/themes/editor/epic-dark.css"
            theme

        let ed = Epic.EpicEditor.Create(opts)

        ed.load()
        |> ignore

    let Main () =
        let container = Div []
        container
        |>! OnAfterRender (fun self ->
            AddEditor self)
