namespace IntelliFactory.WebSharper.TypeScript.TestSite

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html

type T = IntelliFactory.TypeScript.Generator<"../typescript/example1.d.ts">

[<JavaScript>]
module Client =

    [<Inline "A.x">]
    let Ax = Unchecked.defaultof<T.I1>

    let Main () =
        Div [
            P [
                Text "A.x.a = "
                Text (string Ax.a)
            ]
            P [
                Text "A.x.incr(A.x.a, A.x.b) = "
                Text (string (Ax.incr(Ax.a, Ax.b)))
            ]
            P [
                Text "A.x.withRest(A.x.a, A.x.e, A.x.b, \"foo\") = "
                Text (string (Ax.withRest(Ax.a, Ax.e, Ax.b, "foo")))
            ]
        ]
