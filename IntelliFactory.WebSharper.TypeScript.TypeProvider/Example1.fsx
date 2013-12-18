#r "Example1.dll"

let f (x: global.A.B.C.Anon) =
    x.Call()

let f (x: A.B.C.I1) =
    x.incr(x.a, x.b)

let g (x: A.B.C.I1) =
    x.withRest(x.a, x.e, x.b, "test", x.b) |> ignore
    x.withRest(x.a, x.e, [| x.b; "test"; x.b |])

// type T = IntelliFactory.TypeScript.Generator<"../typescript/example1.d.ts">

