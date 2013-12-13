namespace IntelliFactory.WebSharper.TypeScript

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
module C = Contracts

/// Implements assembly generation via System.Reflection.Emit.
module internal ReflectEmit =
    open System.Collections.Generic
