namespace Archie.console
open System
open BenchmarkDotNet.Running
open Archie.core.test


module Consolo =

    [<EntryPoint>]
    let main argv =
        //let summary = BenchmarkRunner.Run<SorterSetFullTest>()
        //printfn "%A" summary

        let qua = new w2()
        qua.wwP()

        Console.ReadKey() |> ignore
        0 // return an integer exit code


