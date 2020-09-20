namespace Archie.console
open System
open Archie.Base

module RunWs =

    let NewRun =
        let filePath = sprintf "c:\log\SorterBnW_%d.txt" System.DateTime.Now.Ticks
        printfn "Starting RunPoolOfBnW"

        let wOrTGen = SwitchOrStage.Stage
        let sorterMutationType = SorterMutationType.Stage (MutationRate.fromFloat 0.05)
        let rngGenSorters = {RngGen.rngType= RngType.Lcg; seed=(RandomSeed.fromInt 2323)}
        let rngGenParams = {RngGen.rngType=RngType.Lcg; seed=(RandomSeed.fromInt 72234)}
        let initialConditionCount = InitialConditionCount.create "" 48 |> Result.ExtractOrThrow
        let replicaCount = ReplicaCount.create "" 48 |> Result.ExtractOrThrow
        let degree = Degree.create "" 12 |> Result.ExtractOrThrow
        let useParallelProcessing = (UseParallel.create false)
        let useEagerProcesing = (UseEagerProc.create false)
        let poolGenCount = PoolGenCount.fromInt 10000
        let poolCount = SorterCount.fromInt 2
        let initialSwitchFrequency = 0.0
        let legacyBias = SorterFitness.fromFloat 0.00

        let res = SorterRun.RunPoolOfBnW
                    filePath
                    initialConditionCount
                    replicaCount
                    degree
                    rngGenSorters
                    wOrTGen
                    initialSwitchFrequency
                    poolGenCount
                    poolCount
                    rngGenParams
                    legacyBias
                    sorterMutationType
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
        let poolTmesGenCount = 100
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

