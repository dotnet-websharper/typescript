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

/// Together with files under `templates` fodler, this module
/// defines Visual Studio templates and extensions for WebSharper.
module VisualStudioIntegration =
    open System

    type NuPkgConfig =
        {
            Id : string
            Path : string
        }

    /// Configuration for the generator.
    type Config =
        {
            /// Path to auto-included NuGet packages (nupkg).
            NuPkgs : list<NuPkgConfig>

            /// Root path to the WebSharper sources.
            RootPath : string

            /// Output path for the `.vsix` file.
            VsixPath : string
        }

    /// Constructs a `.vsix` file with the WebSharper extension.
    val BuildVsixFile : Config -> unit
