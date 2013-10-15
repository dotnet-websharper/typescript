namespace IntelliFactory.WebSharper.TypeScript

module Lexer =
    open System
    open System.Globalization
    open FParsec
    module Names = IntelliFactory.WebSharper.TypeScript.Names

    type Id = Names.Name

    type UserState =
        {
            IdBuilder : Names.NameBuilder
            ReferencePaths : Set<string>
        }

        static member Create(nameBuilder) =
            {
                IdBuilder = nameBuilder
                ReferencePaths = Set.empty
            }

    let addReferencePath path st =
        { st with ReferencePaths = st.ReferencePaths.Add(path) }

    type L<'T> = Parser<'T,UserState>

    let unicodeEscapeSequence : L<string> =
        let hexToInt (c: char) =
            let i = int c
            (i &&& 15) + (i >>> 6) * 9
        let hexEscape =
            pipe4 hex hex hex hex (fun a b c d ->
                let inline ( + ) x y =
                    16 * x + hexToInt y
                string (char (hexToInt a + b + c + d)))
        pchar 'u' >>. hexEscape

    let stringLiteral : L<string> =
        let simpleEscape =
            anyChar
            |>> function
                | 'b' -> "\b"
                | 'f' -> "\u000C"
                | 'n' -> "\n"
                | 'r' -> "\r"
                | 't' -> "\t"
                | c -> string c
        let escape = pchar '\\' >>. (unicodeEscapeSequence <|> simpleEscape)
        let dP c =
            match c with
            | '\\' | '"' -> false
            | _ -> not (Char.IsControl(c))
        let sP c =
            match c with
            | '\\' | '\'' -> false
            | _ -> not (Char.IsControl(c))
        let make q isNormal =
            let normChars  = manySatisfy isNormal
            between q q (stringsSepBy normChars escape)
        let dq = pchar '"'
        let sq = pchar '\''
        make dq dP <|> make sq sP

    let whiteSpace : L<unit> =
        let nextLine =
            skipManySatisfy (fun c -> c <> '\n')
            >>. skipChar '\n'
        let singleLineComment =
            skipString "//" >>. nextLine
        let multiLineComment =
            skipString "/*"
            >>. skipCharsTillString "*/" true Int32.MaxValue
        let refComment =
            let sp = anyOf " \t"
            skipString "///" >>. sp
            >>. skipString "<reference" >>. sp
            >>. skipString "path" >>. sp
            >>. skipString "=" >>. sp
            >>. (stringLiteral >>= (addReferencePath >> updateUserState))
            .>> sp .>> skipString "/>" .>> nextLine
        let comment =
            refComment
            <|> singleLineComment
            <|> multiLineComment
        let space =
            skipMany1Satisfy Char.IsWhiteSpace
            <|> comment
        skipMany space <?> "whitespace"

    let isUnicodeLetter c =
        match Char.GetUnicodeCategory(c) with
        | UnicodeCategory.LowercaseLetter
        | UnicodeCategory.UppercaseLetter
        | UnicodeCategory.TitlecaseLetter
        | UnicodeCategory.ModifierLetter
        | UnicodeCategory.OtherLetter
        | UnicodeCategory.LetterNumber -> true
        | _ -> false

    let charTabulate f =
        let t = [| for c in char 0 .. char 255 -> f c |]
        fun c ->
            let i = int c
            if i < 256 then t.[i] else f c

    let identifierName =
        let isIdentifierStart c =
            match c with
            | '_' | '$' -> true
            | c -> isUnicodeLetter c
        let identifierStart =
            satisfy (charTabulate isIdentifierStart) |>> string
            <|> (pchar '\\' >>. unicodeEscapeSequence)
        let isIdentifierPart c =
            isIdentifierStart c
            || match c with
               | '\u200C' | '\u200D' -> true
               | _ -> false
            || match Char.GetUnicodeCategory(c) with
               | UnicodeCategory.NonSpacingMark
               | UnicodeCategory.SpacingCombiningMark
               | UnicodeCategory.DecimalDigitNumber
               | UnicodeCategory.ConnectorPunctuation -> true
               | _ -> false
        let identifierPart =
            many1Satisfy (charTabulate isIdentifierPart)
            <|> (pchar '\\' >>. unicodeEscapeSequence)
        pipe3
            getUserState
            identifierStart
            (many identifierPart)
            (fun st x xs ->
                let n = String.concat "" (x :: xs)
                st.IdBuilder.CreateName(n))

    let IdentifierName =
        identifierName .>> whiteSpace

    let isReservedWord name =
        match name with
        | "null" | "true" | "false"
        | "break" | "do" | "instanceof" | "typeof"
        | "case" | "else" | "new" | "var"
        | "catch" | "finally" | "return" | "void"
        | "continue" | "for" | "switch" | "while"
        | "debugger" | "function" | "this" | "with"
        | "default" | "if" |"throw"
        | "delete" | "in" | "try"
        | "class" | "enum" | "extends" | "super"
        | "const" | "export" | "import" -> true
        | _ -> false

    let Identifier =
        attempt (identifierName >>= function
            | name when isReservedWord name.Text ->
                fail ("ReservedWord: " + name.Text)
            | name -> preturn name)
        .>> whiteSpace

    let IntegerLiteral : L<int> =
        many1Satisfy Char.IsDigit
        |>> int
        .>> whiteSpace

    let StringLiteral =
        pipe2 getUserState stringLiteral
            (fun st str -> st.IdBuilder.ShareString(str))
        .>> whiteSpace

    let token s = skipString s >>. whiteSpace

    let ``.`` = token "."
    let ``,`` = token ","
    let ``;`` = token ";"
    let ``(`` = token "("
    let ``)`` = token ")"
    let ``{`` = token "{"
    let ``}`` = token "}"
    let ``[`` = token "["
    let ``]`` = token "]"
    let LessThan = token "<"
    let GreaterThan = token ">"
    let Extends = token "extends"
    let Any = token "any"
    let Number = token "number"
    let String = token "string"
    let Void = token "void"
    let TypeOf = token "typeof"
    let Boolean = token "boolean"
    let ``=>`` = token "=>"
    let New = token "new"
    let Interface = token "interface"
    let Import = token "import"
    let Export = token "export"
    let Equal = token "="
    let Require = token "require"
    let ``:`` = token ":"
    let ``?`` = token "?"
    let ``...`` = token "..."
    let Class = token "class"
    let Implements = token "implements"
    let Enum = token "enum"
    let Var = token "var"
    let Function = token "function"
    let Module = token "module"
    let Constructor = token "constructor"
    let Declare = token "declare"
    let Public = token "public"
    let Private = token "private"

    module Flags =

        let flag p =
            (stringReturn p true <|> preturn false)
            .>> whiteSpace

        let ``?`` = flag "?"
        let Export = flag "export"
        let Static = flag "static"

    let Make main =
        whiteSpace >>. main .>> eof
