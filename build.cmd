@ECHO OFF
setlocal
set PATH=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\MSBuild\15.0\Bin;%PATH%
set PATH=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Professional\MSBuild\15.0\Bin;%PATH%
set PATH=%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Enterprise\MSBuild\15.0\Bin;%PATH%
set PATH=%ProgramFiles(x86)%\Microsoft Visual Studio\2019\Community\MSBuild\Current\Bin;%PATH%
set PATH=%ProgramFiles(x86)%\Microsoft Visual Studio\2019\Professional\MSBuild\Current\Bin;%PATH%
set PATH=%ProgramFiles(x86)%\Microsoft Visual Studio\2019\Enterprise\MSBuild\Current\Bin;%PATH%
tools\NuGet\NuGet.exe install FSharp.Core -nocache -version 4.2.3 -excludeVersion -o tools/packages
tools\NuGet\NuGet.exe install IntelliFactory.Build -nocache -pre -excludeVersion -o tools/packages
tools\NuGet\NuGet.exe install FSharp.Compiler.Tools -nocache -version 10.0.2 -excludeVersion -o tools/packages
tools\NuGet\NuGet.exe install FSharp.Core -nocache -version 4.5.0 -o tools/packages
copy tools\fsi.exe.config tools\packages\FSharp.Compiler.Tools\tools\fsi.exe.config
MSBuild.exe msbuild/Main.Zafir.proj /verbosity:minimal /p:VisualStudioVersion=15.0 /p:Arguments="%*" /fileLogger /flp:PerformanceSummary
