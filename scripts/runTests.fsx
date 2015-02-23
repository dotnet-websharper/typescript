#r "../build/Release/Fuchu.dll"
#r "../build/Release/WebSharper.TypeScript.Tests.dll"

open WebSharper.TypeScript.Tests

TestRunner.RunAllTests [||]
|> ignore
