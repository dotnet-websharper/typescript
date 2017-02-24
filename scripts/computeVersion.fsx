#r "../tools/packages/IntelliFactory.Core/lib/net40/IntelliFactory.Core.dll"
#r "../tools/packages/NuGet.Core/lib/net40-Client/NuGet.Core.dll"
#r "../tools/packages/IntelliFactory.Build/lib/net40/IntelliFactory.Build.dll"

let version =
    IntelliFactory.Build.BuildTool()
        .PackageId("Zafir.TypeScript")
        .VersionFrom("Zafir", "alpha")
    |> IntelliFactory.Build.PackageVersion.Full.Find

File.WriteAllText(__SOURCE_DIRECTORY__ + "/../version.txt", version.ToString())
