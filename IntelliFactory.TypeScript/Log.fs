namespace IntelliFactory.TypeScript

open System
open System.Diagnostics

/// Represents the logging facility.
[<AbstractClass>]
type Log() =
    abstract Warn : message: string -> unit
    abstract Warn : message: string * [<ParamArray>] par: obj [] -> unit

    override this.Warn(message: string, [<ParamArray>] par: obj []) =
        this.Warn(String.Format(message, par))

    /// Logs to stderr and System.Diagnsostics.Trace.
    static member Default =
        {
            new Log() with
                member this.Warn(msg) =
                    Trace.WriteLine(msg)
                    stderr.WriteLine(msg)
        }
