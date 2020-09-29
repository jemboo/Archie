namespace Archie.console
open Archie.Base
open System

module Runs = 

    let NextGen<'a,'b> (prams:PoolUpdateParamsBnW)
                       (sorterEx:'a->Sorter)
                       (sorterFitnessEx:'a->SorterFitness)
                       (rewrap: 'a->Sorter->'b)
                       (sorterwraps:'a[]) 
                       (randy:IRando) =

        let mutator = PoolUpdateParamsBnW.mutator prams
        let breederCount = PoolFraction.boundedMultiply prams.breederFrac sorterwraps.Length
        let winnerCount = PoolFraction.boundedMultiply prams.winnerFrac sorterwraps.Length
        let mutantCount = SorterCount.value prams.poolCount - winnerCount

        let stsBreeders = sorterwraps 
                        |> Array.sortByDescending (fun w-> SorterFitness.value (sorterFitnessEx w))
                        |> Array.take(breederCount)
        let stsWinners = sorterwraps
                        |> Array.sortByDescending (fun w-> SorterFitness.value (sorterFitnessEx w))
                        |> Seq.take(winnerCount)
                        |> Seq.map(fun w -> rewrap w (sorterEx w))
                        |> Seq.toArray
        let stsMutants = stsBreeders 
                        |> Combinatorics.drawFromWeightedDistribution 
                                sorterFitnessEx
                                randy
                        |> Seq.take (mutantCount) 
                        |> Seq.map(fun w-> rewrap w (mutator randy (sorterEx w)))
                        |> Seq.toArray
        stsMutants |> Array.append stsWinners


    let getSortingResults (sortableSet:SortableSet) (sorters:Sorter[]) = 
        SorterOps.GetStandardSortingResultsEager sortableSet (UseParallel.create false) sorters 


    let checkArray (a:'a[]) =
        if a.Length < 1 then
            Console.WriteLine ("Array is empty")
            false
        else true


    let RunSorterMpG (sorterInfo:string) 
                     (sorter:Sorter)
                     (res:SorterTestResults)
                     (prams:PoolUpdateParamsBnW) 
                     (reportingFrequency:ReportingFrequency)
                     logToFile =

        //let reportSorterResults (genPool:int) (results:(Sorter*SorterTestResults*SorterFitness)) =
        //    let (srtr,r,f) = results
        //    sprintf "%s %d %d %d %s %.3f %.3f %s %d %d %d" 
        //             sorterInfo 
        //             (SwitchCount.value r.usedSwitchCount) 
        //             (StageCount.value r.stageUseCount)
        //             (SortableCount.value r.successfulSortCount)
        //             prams.fitnessFunc.cat
        //             (PoolFraction.value prams.breederFrac) 
        //             (PoolFraction.value prams.winnerFrac)
        //             (SorterMutationType.StrF prams.mutationType)
        //             (SorterCount.value prams.poolCount) genPool 
        //             (RandomSeed.value prams.rngGen.seed)

        let reportEvalBinsMin (genPool:int) (results:(Sorter*SorterTestResults*SorterFitness)[]) =
            let summary = results|> Array.minBy (fun (srtr,r,f)-> r.usedSwitchCount)
            let (srtr,r,f) = summary
            sprintf "%s %d %d %d %s %.3f %.3f %s %d %d %A" 
                     sorterInfo 
                     (SwitchCount.value r.usedSwitchCount) 
                     (StageCount.value r.stageUseCount)
                     (SortableCount.value r.successfulSortCount)
                     prams.fitnessFunc.cat
                     (PoolFraction.value prams.breederFrac) 
                     (PoolFraction.value prams.winnerFrac)
                     (SorterMutationType.StrF prams.mutationType)
                     (SorterCount.value prams.poolCount) genPool 
                     prams.id

        let nextGenArgs sorterWraps =
            NextGen<Sorter*SorterTestResults*SorterFitness, Sorter>
                       prams
                       (fun (srtr,r,f) -> srtr) 
                       (fun (srtr,r,f) -> f)
                       (fun wrap srtr -> srtr)
                       sorterWraps

        let sortableSet = SortableSet.allBinary sorter.degree |> Result.ExtractOrThrow
        
        let randy = Rando.fromGuid prams.rngType prams.id
        let sortersGen0 = seq {1.. (SorterCount.value prams.poolCount)}
                                |> Seq.map(fun _ -> (PoolUpdateParamsBnW.mutator prams randy) sorter)
                                |> Seq.toArray

        Console.WriteLine(sprintf "%s" sorterInfo)

        let mutable currentEvals = Array.copy(sortersGen0)
        let mutable gen = 0
        let mutable nextRep = 0
        let mutable currentStandardSorterTestResults = res
        while (gen < (GenerationNumber.value prams.runLength)) && (checkArray currentEvals) do
            let genN = GenerationNumber.fromInt gen
            let currentSorterFitness = 
                currentEvals
                |> (getSortingResults sortableSet)
                |> Seq.map(fun r -> (fst r), 
                                    (snd r), 
                                    (prams.fitnessFunc.func (Some (genN:>obj)) (snd r)))
                |> Seq.toArray

           // let _, res, _ = currentSorterFitness

            if (nextRep = (ReportingFrequency.value reportingFrequency)) then 
                let binRec = currentSorterFitness |> reportEvalBinsMin (gen * (SorterCount.value prams.poolCount))
                logToFile binRec true
                nextRep <- 0

            currentEvals <- nextGenArgs currentSorterFitness randy
            gen <- gen + 1
            nextRep <- nextRep + (SorterCount.value prams.poolCount)

        if (checkArray currentEvals) then
            let genN = GenerationNumber.fromInt gen
            let binRecords = currentEvals 
                              |> getSortingResults sortableSet
                              |> Seq.map(fun r -> (fst r), (snd r), (prams.fitnessFunc.func (Some (genN:>obj)) (snd r) ))
                              |> Seq.toArray |> reportEvalBinsMin (gen * (SorterCount.value prams.poolCount))
            logToFile binRecords true
        true


    let RunSorterMpgBatch (logfile:string)
                          (reportingFrequency:int)
                          (paramSeed:int) 
                          (sorterSeed:int)
                          (degree:int)
                          (wOrT:SwitchOrStage)
                          (initialConditionCount:int)
                          (replicaCount:int)
                          (poolTimesGenCount:int) =
        let LogToFile = LogUtils.logFile logfile
        LogToFile "starting RunSorterMpgBatch" false
        let repFreq = ReportingFrequency.create "" reportingFrequency |> Result.ExtractOrThrow
        let icCount = InitialConditionCount.create "" initialConditionCount |> Result.ExtractOrThrow
        let poolGenCount = PoolGenCount.create "" poolTimesGenCount |> Result.ExtractOrThrow

        let replicaCount = ReplicaCount.create "" replicaCount |> Result.ExtractOrThrow
        let degree = Degree.create "" degree |> Result.ExtractOrThrow
        let sorterLength = SorterLength.to999Sucessful degree wOrT
        let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow

        let paramRndGen = RngGen.createLcg paramSeed
        let sorterRando = Rando.LcgFromSeed sorterSeed

        let paramReport =
            sprintf "degree:%d sorterSeed:%d paramSeed:%d sorterLength:%s replicaCount:%d" 
                    (Degree.value degree) sorterSeed paramSeed
                    (sorterLength |> SorterLengthDto.toJson)
                    (ReplicaCount.value replicaCount)

        LogToFile paramReport true
        let reportHeader = "id sw1 st1 sw2 st2 sortT ffT ffP bFrac wFrac mutTy poolCt genPool seed"
        LogToFile reportHeader true


        let RunSorterMpgParams (q:string*Sorter*SorterTestResults) (prams:PoolUpdateParamsBnW) =
            let (info,sorter, res) = q
            RunSorterMpG info sorter res prams repFreq LogToFile
        
        let sorterInfo w t = sprintf "%s %d %d" (string (Guid.NewGuid())) 
                                     (SwitchCount.value w) (StageCount.value t)

        let sorterCount = SorterCount.create "" (InitialConditionCount.value icCount) |> Result.ExtractOrThrow
        let sorters = (SorterSet.createRandom degree sorterLength (Some (SwitchFrequency.fromFloat 0.00)) sorterCount sorterRando).sorters

        let sorterEvals =   sorters
                            |> (getSortingResults sortableSet)
                            |> Seq.map(fun (srtr,r) -> (sorterInfo r.usedSwitchCount r.stageUseCount), srtr, r)
                            |> Seq.toArray

        let sorterMutationType = SorterMutationType.Stage (MutationRate.fromFloat 0.1)
        let legacyBias = SorterFitness.fromFloat 0.00
        let poolCount = SorterCount.fromInt 2
        let sorterAndPrams = PoolUpdateParamsBnW.ParamsSnS paramRndGen poolGenCount poolCount
                                sorterMutationType legacyBias 1024.0 0.0 0.0025 0.0025
                                |> Seq.take (ReplicaCount.value replicaCount)
                                |> Seq.toArray
                                |> Array.allPairs sorterEvals   
                                    
        let _res = sorterAndPrams |> Array.map(fun q -> 
                    let (info, srtr, res), pups = q
                    RunSorterMpgParams (info, srtr, res) pups)

        "RunSorterMpBatch is done"