namespace Archie.console
open System
open Archie.Base

module RunWs =

    let NewRun =
        let filePath = sprintf "c:\log\SorterBnW_%d.txt" System.DateTime.Now.Ticks
        printfn "Starting RunPoolOfBnW"

        let wRtGen = SwitchOrStage.Stage
        let wRtMut = SwitchOrStage.Stage

        let rngGenSorters = {RngGen.rngType= RngType.Lcg; seed=(RandomSeed.fromInt 2323)}
        let rngGenParams = {RngGen.rngType=RngType.Lcg; seed=(RandomSeed.fromInt 72234)}
        let initialConditionCount = InitialConditionCount.create "" 48 |> Result.ExtractOrThrow
        let replicaCount = ReplicaCount.create "" 48 |> Result.ExtractOrThrow
        let degree = Degree.create "" 12 |> Result.ExtractOrThrow
        let mutationRate = MutationRate.fromFloat 0.1
        let useParallelProcessing = (UseParallel.create false)
        let useEagerProcesing = (UseEagerProc.create false)
        let poolGenCount = PoolGenCount.fromInt 100000
        let initialSwitchFrequency = 0.0

        let res = SorterRun.RunPoolOfBnW
                    filePath
                    degree
                    initialSwitchFrequency
                    initialConditionCount
                    mutationRate
                    rngGenParams
                    replicaCount
                    rngGenSorters
                    wRtGen
                    wRtMut
                    poolGenCount
                    useParallelProcessing
                    useEagerProcesing

        Console.WriteLine (sprintf "%s" res)
        //res2 |> List.iter(fun s -> printfn "%s" s)
        Console.ReadKey() |> ignore
        0 // return an integer exit code

    let ClassicRun = 

        let filePath = sprintf "c:\log\SorterB_%d.txt" System.DateTime.Now.Ticks
        printfn "Starting RunSorterMpgBatch"
        let paramSeed = 72234
        let sorterSeed = 2323
        let poolTmesGenCount = 100000
        let replicaCount = 48
        let initialConditionCount = 48
        let degree = 12
        let reportingFrequency = 16
        let wOrT = SwitchOrStage.Stage

        let res = Runs2.RunSorterMpgBatch
                    filePath reportingFrequency
                    paramSeed sorterSeed
                    degree wOrT
                    initialConditionCount
                    replicaCount poolTmesGenCount

        Console.WriteLine (sprintf "%s" res)
        //res2 |> List.iter(fun s -> printfn "%s" s)
        Console.ReadKey() |> ignore
        0 // return an integer exit code

module Consolo =



    [<EntryPoint>]
    let main argv =

        RunWs.NewRun

