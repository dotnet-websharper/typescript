#r "X.dll"

let main () =
    global.FSI_0001.G.f<int>(12);
    global.FSI_0001.G.mapArray((fun x -> x + 1), Array.empty)
