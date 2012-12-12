open System
open Microsoft.CSharp
open System.CodeDom
open System.CodeDom.Compiler

let gen (x: CodeCompileUnit) =
    let provider = CodeDomProvider.CreateProvider("CSharp")
    let opts = CodeGeneratorOptions()
    opts.BracingStyle <- "C"
    opts.IndentString <- "  "
    provider.GenerateCodeFromCompileUnit(x, stdout, opts)

let test () =
    let u = CodeCompileUnit()
    let x = CodeNamespace("Fracking")
    u.Namespaces.Add(x)
    gen u

