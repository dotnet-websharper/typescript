namespace WebSharper.TypeScript
open System
open System.Reflection

module internal Version =

    [<Literal>]
    let BaseVersion = "4.2"

    [<Literal>]
    let Version = BaseVersion + ".0.0"

[<assembly: AssemblyVersion(Version.Version)>]
do ()

