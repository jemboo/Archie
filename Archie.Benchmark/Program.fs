namespace Archie.Benchmark
open System
open BenchmarkDotNet.Running

module Consolo =
    [<EntryPoint>]
    let main argv =
        let summary = BenchmarkRunner.Run<SorterSetRandomTest>()
        printfn "%A" summary
        0 // return an integer exit code
