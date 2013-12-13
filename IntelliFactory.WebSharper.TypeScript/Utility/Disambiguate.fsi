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

/// Hierarchical name disambiguation algorithm that reduces
/// the problem to graph coloring.
module internal Disambiguate =

    /// A `Var` is a placeholder for an integer suffix.
    [<Sealed>]
    type Var =
        interface IComparable

        /// Creates a fresh `Var`.
        static member Create : unit -> Var

    /// Hierarchical name suggestion. 
    type Name =
        | Name1 of string * Var
        | Name2 of Name * string * Var

    /// Defines the disambiguation problem.
    type Problem =
        {
            Names : seq<Name>
        }

    /// Represents a solution, a mapping from `Var` to integers.
    [<Sealed>]
    type Solution =

        /// Looks up the mapping.
        member Item : Var -> int with get

    /// Solves a problem.
    val Solve : Problem -> Solution
