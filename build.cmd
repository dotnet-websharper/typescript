@ECHO OFF
setlocal
set PATH=%PATH%;%ProgramFiles(x86)%\MSBuild\12.0\Bin\
set PATH=%PATH%;%WINDIR%\Microsoft.NET\Framework\v4.0.30319
tools\NuGet\NuGet.exe install IntelliFactory.Build -nocache -pre -excludeVersion -o tools/packages
MSBuild.exe msbuild/Main.proj %*
