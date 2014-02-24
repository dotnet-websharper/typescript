#load "../build/runTests.includes.fsx"
#r "../build/net40/IntelliFactory.WebSharper.TypeScript.dll"
#r "../build/net40/IntelliFactory.WebSharper.TypeScript.Tests.dll"

open IntelliFactory.WebSharper.TypeScript.Tests

TestRunner.RunAllTests [||]
|> ignore
