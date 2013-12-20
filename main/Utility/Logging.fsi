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

module Logging =

    type Level =
        | Verbose
        | Info
        | Warn
        | Error
        | Critical

    [<Sealed>]
    type Message =
        member Level : Level
        member Text : string

    /// Internal logging utility.
    [<Sealed>]
    type internal Logger =

        /// Constructs a new filtering logger for a given level.
        new : Level -> Logger

        /// Reports an arbitrary exception.
        member Exception : exn -> unit

        /// A warning triggered by a failure to resolve a type name.
        member FailedToResolveTypeName : name: string -> unit

        /// System path that were not found.
        member MissingFile : FilePath -> unit

        /// Parser failure on a given file.
        member NoParse : FilePath * string -> unit

        /// Unknown file type (extension) on a given file.
        member UnknownFileType : FilePath -> unit

        /// External module name that could not be resolved.
        member UnresolvedExternalModule : string -> unit

        /// All collected messages.
        member All : seq<Message>

type internal Logger = Logging.Logger

