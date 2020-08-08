namespace Archie.console
open System
open BenchmarkDotNet.Running
open Archie.core.test


module Consolo =

    [<EntryPoint>]
    let main argv =
        let summary = BenchmarkRunner.Run<SorterSetRandomTest>()
        printfn "%A" summary

        //let qua = new w2()
        //qua.ww()

        Console.ReadKey() |> ignore
        0 // return an integer exit code


