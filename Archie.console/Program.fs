namespace Archie.console
open System
//open BenchmarkDotNet.Running


module Consolo =

    [<EntryPoint>]
    let main argv =
        //let summary = BenchmarkRunner.Run<SorterSetRandomTest>()
        //printfn "%A" summary
        Console.WriteLine("Starting")
        let res = Runs.RunSampler2
        printfn "%i" res
        Console.ReadKey() |> ignore
        0 // return an integer exit code


