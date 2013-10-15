namespace IntelliFactory.WebSharper.TypeScript

module Lexer =
    open FParsec

    type Id = Names.Name

    type UserState =
        {
            IdBuilder : Names.NameBuilder
            ReferencePaths : Set<string>
        }

        static member Create : Names.NameBuilder -> UserState

    type L<'T> = Parser<'T,UserState>

    val Identifier : L<Id>
    val IdentifierName : L<Names.Name>
    val StringLiteral : L<string>
    val IntegerLiteral : L<int>

    val ``.`` : L<unit>
    val ``,`` : L<unit>
    val ``;`` : L<unit>
    val ``(`` : L<unit>
    val ``)`` : L<unit>
    val ``{`` : L<unit>
    val ``}`` : L<unit>
    val ``[`` : L<unit>
    val ``]`` : L<unit>
    val LessThan : L<unit>
    val GreaterThan : L<unit>
    val Extends : L<unit>
    val Any : L<unit>
    val Number : L<unit>
    val String : L<unit>
    val Void : L<unit>
    val TypeOf : L<unit>
    val Boolean : L<unit>
    val ``=>`` : L<unit>
    val New : L<unit>
    val Interface : L<unit>
    val Import : L<unit>
    val Export : L<unit>
    val Equal : L<unit>
    val Require : L<unit>
    val ``:`` : L<unit>
    val ``?`` : L<unit>
    val ``...`` : L<unit>
    val Class : L<unit>
    val Implements : L<unit>
    val Enum : L<unit>
    val Var : L<unit>
    val Function : L<unit>
    val Module : L<unit>
    val Constructor : L<unit>
    val Declare : L<unit>
    val Public : L<unit>
    val Private : L<unit>

    module Flags =
        val ``?`` : L<bool>
        val Export : L<bool>
        val Static : L<bool>

    val Make : L<'T> -> L<'T>
