namespace IntelliFactory.WebSharper.TypeScript.TestSite

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html

type T = IntelliFactory.TypeScript.Generator<"../defs/example1.d.ts">

[<JavaScript>]
module Client =

    let Main () =
        T.A.x.a <- 12
        T.A.nestedNumber <- 40
        Div [
            P [
                Text "A.nestedNumber = "
                Text (string T.A.nestedNumber)
            ]
            P [
                Text "A.x.a = "
                Text (string T.A.x.a)
            ]
            P [
                Text "A.x.incr(A.x.a, A.x.b) = "
                Text (string (T.A.x.incr(T.A.x.a, T.A.x.b)))
            ]
            P [
                Text "A.x.withRest(A.x.a, A.x.e, A.x.b, \"foo\") = "
                Text (string (T.A.x.withRest(T.A.x.a, T.A.x.e, T.A.x.b, "foo")))
            ]
        ]
