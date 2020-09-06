namespace Archie.console
open System
//open BenchmarkDotNet.Running


module Consolo =

    [<EntryPoint>]
    let main argv =
        //let summary = BenchmarkRunner.Run<SorterSetRandomTest>()
        //printfn "%A" summary

        let filePath = sprintf "c:\log\SorterB_%d.txt" System.DateTime.Now.Ticks
        printfn "Starting RunSorterMpgBatch"
        let paramSeed = 72234
        let sorterSeed = 2323
        let poolTmesGenCount = 4000
        let replicaCount = 48
        let initialConditionCount = 48
        let degree = 14
        let reportingFrequency = 128
        let switchOrStage = "Switch"

        let res = Runs2.RunSorterMpgBatch
                    filePath reportingFrequency
                    paramSeed sorterSeed
                    degree switchOrStage
                    initialConditionCount
                    replicaCount poolTmesGenCount

        Console.WriteLine (sprintf "%s" res)
        //res2 |> List.iter(fun s -> printfn "%s" s)
        Console.ReadKey() |> ignore
        0 // return an integer exit code


