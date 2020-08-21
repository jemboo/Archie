namespace Archie.console
open System
//open BenchmarkDotNet.Running


module Consolo =

    [<EntryPoint>]
    let main argv =
        //let summary = BenchmarkRunner.Run<SorterSetRandomTest>()
        //printfn "%A" summary
        printfn "Starting RunSorterMpgBatch"

        let res = Runs.RunSorterMpgBatch "c:\log\RunSorterMpgsBatchI.txt"
        Console.WriteLine (sprintf "%s" res)
        //res2 |> List.iter(fun s -> printfn "%s" s)
        Console.ReadKey() |> ignore
        0 // return an integer exit code


