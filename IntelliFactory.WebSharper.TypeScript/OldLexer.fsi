/// Implements lexical analysis of TypeScript 0.8 declarations (.d.ts) files.
module internal IntelliFactory.TypeScript.Lexer

module S = Syntax

type Lexer<'T> = Parser<'T,string,char,list<string>,int>

type LexemeOptions =
    | NewLine
    | Regular

type Keyword =
    | KeywordAny
    | KeywordBool
    | KeywordClass
    | KeywordConstructor
    | KeywordDeclare
    | KeywordEnum
    | KeywordExport
    | KeywordExtends
    | KeywordFunction
    | KeywordImplements
    | KeywordImport
    | KeywordInterface
    | KeywordModule
    | KeywordNew
    | KeywordNumber
    | KeywordPrivate
    | KeywordPublic
    | KeywordStatic
    | KeywordString
    | KeywordVar
    | KeywordVoid

    member ToText : unit -> string

type Token =
    | TokenColon
    | TokenComma
    | TokenCurlyClose
    | TokenCurlyOpen
    | TokenDot
    | TokenDoubleArrow
    | TokenEllipsis
    | TokenEquals
    | TokenIdentifier of S.Identifier
    | TokenInt of int
    | TokenKeyword of Keyword
    | TokenParenClose
    | TokenParenOpen
    | TokenQuestion
    | TokenSemicolon
    | TokenSquareClose
    | TokenSquareOpen
    | TokenStringLiteral of string

[<NoComparison>]
[<Struct>]
[<StructuralEquality>]
type Lexeme =
    new : LexemeOptions * SourcePosition * Token -> Lexeme
    member Options : LexemeOptions
    member SourcePosition : SourcePosition
    member Token : Token

val Lex : Lexer<Lexeme []>
