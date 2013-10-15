
namespace IntelliFactory.WebSharper.TypeScript

#nowarn "40"

module Parser =
    open System
    open FParsec
    module S = Syntax

    type UserState = Lexer.UserState
    type Parser<'T> = Parser<'T,UserState>
    val DeclarationSourceFile : Parser<S.DeclarationSourceFile>
