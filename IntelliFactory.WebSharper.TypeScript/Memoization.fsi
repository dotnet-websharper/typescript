namespace IntelliFactory.WebSharper.TypeScript

/// Implements memoization tables.
module internal Memoization =
    open System
    open System.Collections.Generic

    type Options<'T1,'T2> =
        {
            Equality : IEqualityComparer<'T1>
            Laziness : option<Lazy<'T2> -> 'T2>
        }

    module Options =
        val Default<'T1,'T2> : Options<'T1,'T2>

    val Configure : unit -> Options<'T1,'T2>

    [<Sealed>]
    type Table<'T1,'T2> =
        member Clear : unit -> unit
        member Get : 'T1 -> 'T2
        member GetKeys : unit -> seq<'T1>
        member Remove : 'T1 -> bool
        member Count : int
        member Equality : IEqualityComparer<'T1>
        member Item : 'T1 -> 'T2 with get

    val Memoize<'T1,'T2> :
        Options<'T1,'T2> ->
        ('T1 -> 'T2) ->
        Table<'T1,'T2>

    val MemoizeRecursive<'T1,'T2> :
        Options<'T1,'T2> ->
        (('T1 -> 'T2) -> ('T1 -> 'T2)) ->
        Table<'T1,'T2>

    type Options<'T1,'T2> with
        member Memoize : ('T1 -> 'T2) -> Table<'T1,'T2>
        member MemoizeRecursive :
            (('T1 -> 'T2) -> ('T1 -> 'T2)) ->
            Table<'T1,'T2>
