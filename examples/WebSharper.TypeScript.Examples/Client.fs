namespace WebSharper.TypeScript.Examples

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
type Epic = WebSharper.EpicEditor

[<JavaScript>]
module Client =

    let AddEditor (el: Html.Element) =
        let opts =
            Epic.EpicEditorOptions
                (
                    container = el.Dom,
                    file =
                        /// NOTE: need better name than Anon.
                        Epic.Anon(name = "MyDocument", defaultContent = "# My Document", autoSave = 100),
                    basePath =  "/EpicEditor-v0.2.2",
                    theme =
                        Epic.Anon1(_base = "/themes/base/epiceditor.css",
                            preview = "/themes/preview/preview-dark.css",
                            editor = "/themes/editor/epic-dark.css")
                )
        let ed = Epic.EpicEditor.Create(opts)
        ed.load()
        |> ignore

    let Main () =
        let container = Div []
        container
        |>! OnAfterRender (fun self ->
            AddEditor self)
