namespace Archie.console
open System
open BenchmarkDotNet.Running
open Archie.core.test


module Consolo =

    [<EntryPoint>]
    let main argv =
        let summary = BenchmarkRunner.Run<SorterReactionTest>()
        printfn "%A" summary
        Console.ReadKey() |> ignore
        0 // return an integer exit code
