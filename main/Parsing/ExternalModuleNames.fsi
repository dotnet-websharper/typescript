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

namespace IntelliFactory.WebSharper.TypeScript

/// Implements section 11.2.1 of of TypeScript manual 0.9.5.
module internal ExternalModuleNames =

    /// Represents a relative external module name.
    [<Sealed>]
    type RelativeName =
        interface IComparable

        /// The normalized textual form.
        member Text : string

    /// Represents a top-level external module name.
    [<Sealed>]
    type TopLevelName =
        interface IComparable

        /// The normalized textual form.
        member Text : string

    /// Represents a module name.
    type Name =
        | Relative of RelativeName
        | TopLevel of TopLevelName

        /// The normalized textual form.
        member Text : string

        /// Parses a name from a givem string.
        static member Parse : Names.NameBuilder * string -> Name

        /// Parses a name from a givem string.
        static member Parse : string -> Name

        /// Attemtps to parse a name.
        static member TryParse : string -> option<Name>

        /// Parses a name from a givem string.
        static member TryParse : Names.NameBuilder * string -> option<Name>
