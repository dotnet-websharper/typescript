@ECHO OFF
setlocal
set PATH=%PATH%;%ProgramFiles(x86)%\MSBuild\14.0\Bin\
set PATH=%PATH%;%WINDIR%\Microsoft.NET\Framework\v4.0.30319
MSBuild.exe msbuild/Main.Zafir.proj %*
