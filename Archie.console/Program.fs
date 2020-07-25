namespace Archie.console
open System
//open Archie.core
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open Archie.core.test

//open System.Security.Cryptography
//open System


module Consolo =

    [<EntryPoint>]
    let main argv =
        let summary = BenchmarkRunner.Run<PrivateTrackerTest>()
        printfn "%A" summary
        Console.ReadKey() |> ignore
        0 // return an integer exit code
