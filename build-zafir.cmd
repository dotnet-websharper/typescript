@ECHO OFF
setlocal
set PATH=%PATH%;%ProgramFiles(x86)%\MSBuild\14.0\Bin\
set PATH=%PATH%;%WINDIR%\Microsoft.NET\Framework\v4.0.30319
tools\NuGet\NuGet.exe install IntelliFactory.Build -nocache -pre -excludeVersion -o tools/packages
tools\NuGet\NuGet.exe install FSharp.Compiler.Tools -nocache -version 4.0.1.21 -excludeVersion -o tools/packages
copy tools\fsi.exe.config tools\packages\FSharp.Compiler.Tools\tools\fsi.exe.config
MSBuild.exe msbuild/Main.Zafir.proj %*
