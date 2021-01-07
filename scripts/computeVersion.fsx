#r "../tools/packages/IntelliFactory.Core/lib/net40/IntelliFactory.Core.dll"
#r "../tools/packages/NuGet.Core/lib/net40-Client/NuGet.Core.dll"
#r "../tools/packages/IntelliFactory.Build/lib/net40/IntelliFactory.Build.dll"

open System.IO
open IntelliFactory.Build

let version =
    BuildTool()
        .PackageId("WebSharper.TypeScript")
        .VersionFrom("WebSharper", "", "[4.0,5.0)")
    |> PackageVersion.Full.Find

File.WriteAllText(__SOURCE_DIRECTORY__ + "/../build/version.txt", version.ToString())
