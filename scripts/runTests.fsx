#r "../build/Release/Fuchu.dll"
#r "../build/Release/IntelliFactory.WebSharper.TypeScript.Tests.dll"

open IntelliFactory.WebSharper.TypeScript.Tests

TestRunner.RunAllTests [||]
|> ignore
