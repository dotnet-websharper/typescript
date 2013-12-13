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

module G = GraphColoring

module Disambiguate =

    type Var =
        | Var of int64

    let mutable varCounter = 0L

    type Var with
        static member Create() =
            let i = Interlocked.Increment(&varCounter)
            Var i

    type Name =
        | Name1 of string * Var
        | Name2 of Name * string * Var

    module Map =

        let update key upd map =
            Map.add key (upd (Map.tryFind key map)) map

    /// Used to reprsent a `seq<N>` as a trie, grouping related `Var`s together.
    module Trie =

        type T =
            | T1 of Map<string,Set<Var> * T>

        let empty =
            T1 Map.empty

        let add n t =
            let rec add n k (T1 map as t) =
                match n with
                | Name1 (name, var) ->
                    let update state =
                        match state with
                        | None -> (Set.singleton var, k empty)
                        | Some (vars, st) -> (Set.add var vars, k st)
                    T1 (Map.update name update map)
                | Name2 (parent, name, var) ->
                    add parent (add (Name1 (name, var)) k) t
            add n (fun x -> x) t

        let create ns =
            Seq.fold (fun t n -> add n t) empty ns

        let vars t =
            let rec visit (T1 map) =
                seq {
                    for (KeyValue (_, (varSet, sub))) in map do
                        yield! varSet
                        yield! visit sub
                }
            Seq.distinct (visit t)

        let rec varSets (T1 map) =
            seq {
                for KeyValue (k, (vs, t)) in map do
                    yield vs
                    yield! varSets t
            }

    type Problem =
        {
            Names : seq<Name>
        }

    [<Sealed>]
    type Solution(d: IDictionary<Var,int>) =
        member __.Item with get (v) = d.[v]

    let Solve problem =
        let t = Trie.create problem.Names
        let nodes =
            dict <| seq {
                for var in Trie.vars t ->
                    (var, G.CreateNode var)
            }
        let edges =
            seq {
                for varSet in Trie.varSets t do
                    let vars = Set.toArray varSet
                    for i in 0 .. vars.Length - 2 do
                        for j in i + 1 .. vars.Length - 1 do
                            yield (nodes.[vars.[i]], nodes.[vars.[j]])
            }
        let coloring =
            G.CreateGraph nodes.Values edges
            |> G.ComputeColoring
        let result =
            dict <| seq {
                for node in nodes.Values do
                    yield (G.Label node, G.Color coloring node)
            }
        Solution(result)
