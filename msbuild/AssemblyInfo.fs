namespace WebSharper.TypeScript
open System
open System.Reflection

module internal Version =

    [<Literal>]
#if ZAFIR
    let BaseVersion = "4.0"
#else
    let BaseVersion = "3.6"
#endif

    [<Literal>]
    let Version = BaseVersion + ".0.0"

[<assembly: AssemblyVersion(Version.Version)>]
do ()

