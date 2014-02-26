namespace WebSharper.TypeScript.Examples

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
type Epic = WebSharper.EpicEditor

[<JavaScript>]
module Client =

    let AddEditor (el: Html.Element) =
        Epic.EpicEditorOptions(container = el.Dom,
            file =  Epic.Misc.file(name = "MyDocument",
                        defaultContent = "# My Document",
                        autoSave = 100),
            basePath =  "/EpicEditor-v0.2.2",
            theme = Epic.Misc.theme(_base = "/themes/base/epiceditor.css",
                        preview = "/themes/preview/preview-dark.css",
                        editor = "/themes/editor/epic-dark.css"))
        |> Epic.EpicEditor.Create
        |> fun ed -> ed.load() |> ignore

    let Main () =
        Div []
        |>! OnAfterRender (fun self -> AddEditor self)
