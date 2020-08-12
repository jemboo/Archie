namespace Archie.console
open System
//open BenchmarkDotNet.Running


module Consolo =

    [<EntryPoint>]
    let main argv =
        //let summary = BenchmarkRunner.Run<SorterSetRandomTest>()
        //printfn "%A" summary
        printfn "Starting"
        //let res = Runs.RunSampler
        //res |> List.iter(fun s -> printfn "%s" s)

        //let res2 = Runs.RunSampler2
        //res2 |> List.iter(fun s -> printfn "%s" s)
        Console.ReadKey() |> ignore
        0 // return an integer exit code


