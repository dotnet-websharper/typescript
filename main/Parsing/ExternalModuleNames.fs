// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
//
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

namespace WebSharper.TypeScript

open FParsec

module ExternalModuleNames =

    type Loc<'T> =
        | Down of Loc<'T> * string
        | Up of 'T

    let writeLoc (w: TextWriter) interp loc =
        let writeUp k =
            if k > 0 then
                w.Write("..")
            for _ in 1 .. k - 1 do
                w.Write("/..")
        let rec wr loc =
            match loc with
            | Down (Up up, n) ->
                match interp up with
                | k when k < 0 -> w.Write(n)
                | 0 -> w.Write("./"); w.Write(n)
                | k -> writeUp k; w.Write("/"); w.Write(n)
            | Down (loc, n) ->
                wr loc
                w.Write('/')
                w.Write(n)
            | Up up ->
                match interp up with
                | 0 -> w.Write(".")
                | k -> writeUp k
        wr loc

    let showLoc interp loc =
        use w = new StringWriter()
        writeLoc w interp loc
        w.ToString()

    type RelativeName =
        {
            RelLoc : Loc<int>
            RNT : string
        }

        override n.ToString() = n.RNT
        member n.Text = n.RNT

        static member Create(loc) =
            {
                RelLoc = loc
                RNT = showLoc (fun x -> x) loc
            }

    type TopLevelName =
        {
            TopLoc : Loc<unit>
            TLT : string
        }

        override n.ToString() = n.TLT
        member n.Text = n.TLT

        static member Create(loc) =
            {
                TopLoc = loc
                TLT = showLoc (fun () -> -1) loc
            }

    type Name =
        | Relative of RelativeName
        | TopLevel of TopLevelName

        member n.Text =
            match n with
            | Relative rN -> rN.Text
            | TopLevel tN -> tN.Text

        override n.ToString() =
            n.Text

    type internal P<'T> = Parser<'T,Names.NameBuilder>

    let rec mapLoc f loc =
        match loc with
        | Down (loc, x) -> Down (mapLoc f loc, x)
        | Up x -> Up (f x)

    let isIdentStart c =
        match c with
        | '_' -> true
        | c when c >= 'a' && c <= 'z' -> true
        | _ -> false

    let isIdentPart c =
        isIdentStart c
        || c >= 'A' && c <= 'Z'
        || c >= '0' && c <= '9'

    let ident : P<string> =
        pipe3
            getUserState
            (satisfy (Char.Tabulate isIdentStart))
            (manySatisfy (Char.Tabulate isIdentPart))
            (fun st x y -> st.ShareString(string x + y))

    let term =
        pstring ".." <|> pstring "." <|> ident

    let terms =
        sepBy1 term (pchar '/')

    let withPath loc path =
        match loc, path with
        | loc, "." -> loc
        | Down (loc, _), ".." -> loc
        | Up n, ".." -> Up (n + 1)
        | loc, path -> Down (loc, path)

    let name =
        terms <?> "external module name"
        |>> fun ts ->
            let rL = List.fold withPath (Up 0) ts
            match ts with
            | "." :: _
            | ".." :: _ ->
                Relative (RelativeName.Create rL)
            | _ ->
                let aL = mapLoc ignore rL
                TopLevel (TopLevelName.Create aL)

    type Name with

        static member TryParse(nb, s) =
            match runParserOnString name nb s s with
            | Success (name, _, pos) when pos.Index = int64 s.Length -> Some name
            | _ -> None

        static member TryParse(s) =
            let nb = Names.NameBuilder.Create()
            Name.TryParse(nb, s)

        static member Parse(nb, s) =
            match Name.TryParse(nb, s) with
            | None -> failwithf "Not a valid external module name: %s" s
            | Some name -> name

        static member Parse(s) =
            let nb = Names.NameBuilder.Create()
            Name.Parse(nb, s)
