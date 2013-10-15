namespace IntelliFactory.WebSharper.TypeScript

module Names =
    open System
    open System.Collections.Concurrent
    open System.Threading

    let ( == ) (a: obj) (b: obj) =
        Object.ReferenceEquals(a, b)

    [<Sealed>]
    type Name(table: NameBuilder, name: string, stamp: int) =
        inherit Object()
        override x.Equals(o: obj) = base.Equals(o)
        override x.GetHashCode() = stamp
        override x.ToString() = name
        member x.Stamp = stamp
        member x.Table = table
        member x.Text = name

        interface IComparable with
            override x.CompareTo(o: obj) =
                match o with
                | _ when x == o -> 0
                | :? Name as o when table == x.Table -> compare stamp x.Stamp
                | _ -> invalidArg "o" "Invalid comparison"

    and [<Sealed>] NameBuilder() as self =
        let cache = ConcurrentDictionary<string,Name>()
        let mutable stamp = 0

        let makeId =
            Func<string,Name>(fun n ->
                let k = Interlocked.Increment(&stamp)
                Name(self, n, k))

        member t.CreateName(name: string) =
            cache.GetOrAdd(name, makeId)

        /// Rationale for a separate method: we might check
        /// identifiers for some lexical rules, but should not
        /// check arbitrary text which also benefits from hash-consing.
        member t.ShareString(text: string) =
            t.CreateName(text).Text

        static member Create() =
            NameBuilder()

