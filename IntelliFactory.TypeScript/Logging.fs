module IntelliFactory.TypeScript.Logging

open System
open System.Diagnostics

/// The priority of a log message.
type LogLevel =
    | LogError
    | LogInformation
    | LogVerbose
    | LogWarning

    override this.ToString() =
        match this with
        | LogError -> "Error"
        | LogInformation -> "Info"
        | LogVerbose -> "Verbose"
        | LogWarning -> "Warn"

/// Represents the logging facility.
[<AbstractClass>]
type Log() =

    abstract Send : level: LogLevel * message: string -> unit
    abstract Send : level: LogLevel * message: string * [<ParamArray>] args: obj [] -> unit

    override this.Send(level, message, args) =
        this.Send(level, String.Format(message, args))

    member this.Error(message: string) =
        this.Send(LogError, message)

    member this.Error(message: string, [<ParamArray>] args: obj []) =
        this.Send(LogError, message, args)

    member this.Information(message: string) =
        this.Send(LogInformation, message)

    member this.Information(message: string, [<ParamArray>] args: obj []) =
        this.Send(LogInformation, message, args)

    member this.Verbose(message: string) =
        this.Send(LogVerbose, message)

    member this.Verbose(message: string, [<ParamArray>] args: obj []) =
        this.Send(LogVerbose, message, args)

    member this.Warning(message: string) =
        this.Send(LogWarning, message)

    member this.Warning(message: string, [<ParamArray>] args: obj []) =
        this.Send(LogWarning, message, args)

    member this.Time<'T>(name: string)(action: unit -> 'T) : 'T =
        let sw = Stopwatch()
        sw.Start()
        let r = action ()
        this.Information("{0} completed in {1} ms", name, sw.ElapsedMilliseconds)
        r

    /// Ignores all logging information.
    static member Default =
        {
            new Log() with
                override this.Send(level, msg) = ()
                override this.Send(level, msg, args) = ()
        }
