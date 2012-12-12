module internal IntelliFactory.TypeScript.Declarations.Lexer

module S = Syntax
open System
open IntelliFactory.Parsec
module P = IntelliFactory.Parsec.Parser
module S = Syntax

type Lexer<'T> = Parser<'T,string,char,list<string>,int>

type LexemeOptions =
    | NewLine
    | Regular

    override this.ToString() =
        match this with
        | NewLine -> "NewLine"
        | Regular -> "Regular"

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

    override this.ToString() =
        this.ToText()

    member this.ToText() =
        match this with
        | KeywordAny         -> "any"
        | KeywordBool        -> "bool"
        | KeywordClass       -> "class"
        | KeywordConstructor -> "constructor"
        | KeywordDeclare     -> "declare"
        | KeywordEnum        -> "enum"
        | KeywordExport      -> "export"
        | KeywordExtends     -> "extends"
        | KeywordFunction    -> "function"
        | KeywordImplements  -> "implements"
        | KeywordImport      -> "import"
        | KeywordInterface   -> "interface"
        | KeywordModule      -> "module"
        | KeywordNew         -> "new"
        | KeywordNumber      -> "number"
        | KeywordPrivate     -> "private"
        | KeywordPublic      -> "public"
        | KeywordStatic      -> "static"
        | KeywordString      -> "string"
        | KeywordVar         -> "var"
        | KeywordVoid        -> "void"

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
    | TokenKeyword of Keyword
    | TokenParenClose
    | TokenParenOpen
    | TokenQuestion
    | TokenSemicolon
    | TokenSquareClose
    | TokenSquareOpen
    | TokenStringLiteral of string

    override this.ToString() =
        match this with
        | TokenColon -> ":"
        | TokenComma -> ","
        | TokenCurlyClose -> "}"
        | TokenCurlyOpen -> "{"
        | TokenDot -> "."
        | TokenDoubleArrow -> "=>"
        | TokenEllipsis -> "..."
        | TokenEquals -> "="
        | TokenIdentifier (S.Identifier id) -> id
        | TokenKeyword kw -> string kw
        | TokenParenClose -> ")"
        | TokenParenOpen -> "("
        | TokenQuestion -> "?"
        | TokenSemicolon -> ";"
        | TokenSquareClose -> "]"
        | TokenSquareOpen -> "["
        | TokenStringLiteral s -> String.Format(@"""{0}""", s)

[<Struct>]
type Lexeme(opts: LexemeOptions, pos: SourcePosition, token: Token) =
    member this.Options = opts
    member this.SourcePosition = pos
    member this.Token = token

    override this.ToString() =
        String.Format("{0} at {1}", string token, pos)

type L<'T> = Lexer<'T>

let ( <*> ) f x = P.Map2 (fun f x -> f x) f x
let ( <^> ) f x = P.Map f x
let ( <|> ) a b = P.Or a b
let ( >*> ) a b = P.Combine a b
let ( <*< ) a b = a |> P.Before b
let ( <=> ) a b = b >*> P.Yield a

let manyChars p =
    P.Many p
    |> P.Map (fun chars -> new System.String(Seq.toArray chars))

let fix f =
    let rec g = P.Delay (fun () -> f g)
    g

let lexHexEscape : L<char> =
    let d =
        P.Satisfy (fun c ->
            Char.IsDigit c
            || c >= 'a' && c <= 'f'
            || c >= 'A' && c <= 'F')
    let ( <.> ) x y = 16 * x + y
    let n x =
        if x >= '0' && x <= '9' then
            int x - int '0'
        elif x >= 'a' && x <= 'f' then
            int x - int 'a' + 10
        else
            int x - int 'A' + 10
    let f d1 d2 d3 d4 =
        char (n d1 <.> n d2 <.> n d3 <.> n d4)
    f <^> d <*> d <*> d <*> d

let lexEscape : L<char> =
    let pB = P.Yield '\b'
    let pF = P.Yield '\f'
    let pN = P.Yield '\n'
    let pR = P.Yield '\r'
    let pT = P.Yield '\t'
    let f c =
        match c with
        | 'b' -> pB
        | 'f' -> pF
        | 'n' -> pN
        | 'r' -> pR
        | 't' -> pT
        | 'u' -> lexHexEscape
        | c   -> P.Yield c
    P.Char '\\' >*> P.Bind P.AnyToken f

let lexRawStringLiteral : L<string> =
    let str q p = P.Between q q (manyChars (P.Satisfy p <|> lexEscape))
    let dP c =
        match c with
        | '\\' | '"' -> false
        | _ -> not (Char.IsControl(c))
    let sP c =
        match c with
        | '\\' | '\'' -> false
        | _ -> not (Char.IsControl(c))
    let single = str (P.Char '"') dP
    let double = str (P.Char '\'') sP
    (double <|> single)

let lexStringLiteral : L<Token> =
    TokenStringLiteral <^> lexRawStringLiteral

let lexRefPathComment : L<unit> =
    let ws = P.SkipMany (P.Satisfy Char.IsWhiteSpace)
    let w1 = P.SkipMany1 (P.Satisfy Char.IsWhiteSpace)
    let p =
        (
            P.String "///"
            >*> ws
            >*> P.String "<reference"
            >*> w1
            >*> P.String "path"
            >*> ws
            >*> P.String "="
            >*> ws
            >*> lexRawStringLiteral
            <*< P.SkipMany (P.Satisfy (fun c -> c <> '\n'))
        )
    P.Bind p (fun x -> P.ModifyState (fun xs -> x :: xs))

let lexWhiteSpace : L<unit> =
    let not c = P.Satisfy ((<>) c)
    let white = P.SkipMany1 (P.Satisfy Char.IsWhiteSpace)
    let singleComment = P.String "//" >*> P.SkipMany (not '\n')
    let inMultiComment : L<unit> =
        fix <| fun inComment ->
            P.String "*/"
            <|> (P.SkipMany1 (not '*') >*> inComment)
            <|> (P.SkipMany1 (P.Char '*') >*> (P.Ignore (P.Char '/') <|> inComment))
    let multiComment =
        P.String "/*" >*> inMultiComment
    let p = white <|> lexRefPathComment <|> singleComment <|> multiComment
    P.SkipMany p

let lexWord : L<Token> =
    let isStart c =
        match c with
        | '$' | '_' -> true
        | _ -> Char.IsLetter(c)
    let isPart c =
        match c with
        | '$' | '_' -> true
        | _ -> Char.IsLetterOrDigit(c)
    let tok x =
        match x with
        | "any"         -> TokenKeyword KeywordAny
        | "bool"        -> TokenKeyword KeywordBool
        | "class"       -> TokenKeyword KeywordClass
        | "constructor" -> TokenKeyword KeywordConstructor
        | "declare"     -> TokenKeyword KeywordDeclare
        | "enum"        -> TokenKeyword KeywordEnum
        | "export"      -> TokenKeyword KeywordExport
        | "extends"     -> TokenKeyword KeywordExtends
        | "function"    -> TokenKeyword KeywordFunction
        | "implements"  -> TokenKeyword KeywordImplements
        | "import"      -> TokenKeyword KeywordImport
        | "interface"   -> TokenKeyword KeywordInterface
        | "module"      -> TokenKeyword KeywordModule
        | "new"         -> TokenKeyword KeywordNew
        | "number"      -> TokenKeyword KeywordNumber
        | "private"     -> TokenKeyword KeywordPrivate
        | "public"      -> TokenKeyword KeywordPublic
        | "static"      -> TokenKeyword KeywordStatic
        | "string"      -> TokenKeyword KeywordString
        | "var"         -> TokenKeyword KeywordVar
        | "void"        -> TokenKeyword KeywordVoid
        | x             -> TokenIdentifier (S.Identifier x)
    let mk c cs = tok (string c + cs)
    mk <^> P.Satisfy isStart <*> manyChars (P.Satisfy isPart)

let lexPunctuation : L<Token> =
    let isPunctuation c =
        match c with
        | '(' | ')' | ',' | '.' | ':' | ';'
        | '=' | '?' | '[' | ']' | '{' | '}' -> true
        | _ -> false
    let dot =
        (TokenEllipsis <=> P.String "..")
        <|> P.Yield TokenDot
    let equals =
        (TokenDoubleArrow <=> P.Char '>')
        <|> P.Yield TokenEquals
    let parenOpen = P.Yield TokenParenOpen
    let parenClose = P.Yield TokenParenClose
    let comma = P.Yield TokenComma
    let colon = P.Yield TokenColon
    let semicolon = P.Yield TokenSemicolon
    let question = P.Yield TokenQuestion
    let squareOpen = P.Yield TokenSquareOpen
    let squareClose = P.Yield TokenSquareClose
    let curlyOpen = P.Yield TokenCurlyOpen
    let curlyClose = P.Yield TokenCurlyClose
    P.Bind (P.Satisfy isPunctuation) (fun c ->
        match c with
        | '(' -> parenOpen
        | ')' -> parenClose
        | ',' -> comma
        | '.' -> dot
        | ':' -> colon
        | ';' -> semicolon
        | '=' -> equals
        | '?' -> question
        | '[' -> squareOpen
        | ']' -> squareClose
        | '{' -> curlyOpen
        | '}' -> curlyClose
        | _   -> P.Zero)

let lexToken : L<Token> =
    lexStringLiteral
    <|> lexWord
    <|> lexPunctuation

let skipBOM : L<unit> =
    P.SkipMany (P.Char (char 65279))

let Lex : L<Lexeme []> =
    let p = (fun pos tok -> Lexeme(Regular, pos, tok)) <^> P.GetPosition <*> lexToken
    (skipBOM >*> lexWhiteSpace >*> P.Many (p <*< lexWhiteSpace) <*< P.EndOfInput)
    |> P.Map (fun lexemes ->
        [|
            let current = ref 0
            for lexeme in lexemes do
                let line = lexeme.SourcePosition.Line
                if line > !current then
                    do current := line
                    yield Lexeme(NewLine, lexeme.SourcePosition, lexeme.Token)
                else
                    yield lexeme
        |])
