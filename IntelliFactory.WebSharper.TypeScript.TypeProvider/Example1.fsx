#r "Example1.dll"

let f (x: global.A.B.C.Anon) =
    x.Call()

let f (x: A.B.C.I1) =
    x.incr(x.a, x.b)

// type T = IntelliFactory.TypeScript.Generator<"../typescript/example1.d.ts">

