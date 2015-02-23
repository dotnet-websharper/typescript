// Copyright 2013 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.

namespace WebSharper.TypeScript

/// Defines VisualStudio templates for WebSharper.
module VisualStudioIntegration =
    open System
    open System.IO
    open System.Text.RegularExpressions
    module X = XmlTools
    module NG = NuGet
    module VST = Templates
    module VX = Extensions
    type Content = Utils.Content

    let getExtensionName () =
        "WebSharper.TypeScript"

    let getExtensionGuid () =
        Guid("d01cc8af-94c1-4f1d-90b3-620328864a91")

    let getExtensionDecription () =
        "WebSharper support for cross-compiling TypeScript definition files"

    let pattern = Regex(@"(\d+(\.\d+)*)(\-(\w+))?$")

    type VersionInfo =
        {
            FullVersion : string
            NumericVersion : Version
            PackageId : string
            VersionSuffix : option<string>
        }

        static member FromFileName(package: string) =
            let fn = Path.GetFileNameWithoutExtension(package)
            let m = pattern.Match(fn)
            if m.Success then
                Some {
                    FullVersion = m.Groups.[0].Value
                    NumericVersion = Version(m.Groups.[1].Value)
                    PackageId = fn.Substring(0, m.Groups.[1].Index - 1)
                    VersionSuffix =
                        match m.Groups.[4].Value with
                        | "" -> None
                        | v -> Some v
                }
            else None

    type NuPkgConfig =
        {
            Id : string
            Path : string
        }

    type Config =
        {
            NuPkgs : list<NuPkgConfig>
            RootPath : string
            VsixPath : string
        }

    let ( +/ ) a b =
        Path.Combine(a, b)

    type Common =
        {
            Config : Config
            Icon : VST.Icon
            VersionInfo : VersionInfo
        }

        static member Create(cfg) =
            let iconPath = cfg.RootPath +/ "WebSharper.png"
            let icon = VST.Icon.FromFile(iconPath)
            {
                Config = cfg
                Icon = icon
                VersionInfo = VersionInfo.FromFileName(cfg.NuPkgs.Head.Path).Value
            }

    let getIdentity () =
        VST.ExtensionIdentity.Create(getExtensionName (), getExtensionGuid ())

    let makeProjectTemplate com meta project =
        let identity = getIdentity ()
        let pkgs =
            [
                for nuPkg in com.Config.NuPkgs do
                    let c = Content.ReadBinaryFile(nuPkg.Path)
                    let vn = com.VersionInfo
                    yield NG.Package.Create(nuPkg.Id, vn.FullVersion, c)
            ]
        let nuGet = VST.NuGetPackages.Create(identity, pkgs)
        VST.ProjectTemplate.Create(meta, project)
            .WithNuGetPackages(nuGet)

    let makeTemplateMetadata com name dpn desc =
        VST.TemplateData.Create(VST.ProjectType.FSharp,
            name = name,
            description = desc,
            icon = com.Icon)
            .WithDefaultProjectName(dpn)

    let getTypeScriptLibraryTemplate com =
        let dir = com.Config.RootPath +/ "templates" +/ "ts-library"
        let meta =
            "WebSharper template for cross-compiling TypeScript definition files"
            |> makeTemplateMetadata com "TypeScript Library" "TypeScriptLibrary"
        let file name =
            let i = VST.ProjectItem.FromTextFile(dir +/ name).ReplaceParameters()
            VST.FolderElement.Nested(i)
        let project =
            VST.Project.FromFile(dir +/ "TypeScriptLibrary.fsproj",
                [
                    file "Main.d.ts"
                ])
                .ReplaceParameters()
        makeProjectTemplate com meta project

    let getWebSharperTypeScriptExtension com =
        let desc = getExtensionDecription ()
        let editions =
            [
                VX.VSEdition.Premium
                VX.VSEdition.Pro
                VX.VSEdition.Ultimate
                VX.VSEdition.VWDExpress
            ]
        let products =
            [
                for v in ["10.0"; "11.0"; "12.0"] do
                    yield VX.VSProduct.Create(v, editions).AsSupportedProduct()
            ]
        let identifier =
            VX.Identifier.Create("IntelliFactory", getIdentity (), com.VersionInfo.PackageId, desc)
                .WithVersion(com.VersionInfo.NumericVersion)
                .WithProducts(products)
        let category = ["WebSharper"]
        let proj x = VX.VsixContent.ProjectTemplate(category, x)
        let vsix =
            VX.Vsix.Create(identifier,
                [
                    proj (getTypeScriptLibraryTemplate com)
                ])
        VX.VsixFile.Create(Path.GetFileName(com.Config.VsixPath), vsix)

    let BuildVsixFile cfg =
        let com = Common.Create(cfg)
        let ext = getWebSharperTypeScriptExtension com
        ext.WriteToDirectory(Path.GetDirectoryName(cfg.VsixPath))

