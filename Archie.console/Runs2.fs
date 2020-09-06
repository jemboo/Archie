namespace Archie.console
open Archie.Base
open System

module Runs2 = 

    let NextGen<'a,'b> (mutator:Sorter->Sorter)
                       (poolCount:SorterCount)
                       (breederFrac:PoolFraction)
                       (winnerFrac:PoolFraction)
                       (randy:IRando)
                       (sorterEx:'a->Sorter)
                       (sorterFitnessEx:'a->SorterFitness)
                       (rewrap: 'a->Sorter->'b)
                       (sorterwraps:'a[]) =

        let stsBreeders = sorterwraps 
                        |> Array.sortByDescending (fun w-> SorterFitness.value (sorterFitnessEx w))
                        |> Array.take(PoolFraction.boundedMultiply breederFrac sorterwraps.Length)
        let stsWinners = sorterwraps
                        |> Array.sortByDescending (fun w-> SorterFitness.value (sorterFitnessEx w))
                        |> Seq.take(PoolFraction.boundedMultiply winnerFrac sorterwraps.Length)
                        |> Seq.map(fun w -> rewrap w (sorterEx w) )
                        |> Seq.toArray
        let stsMutants = stsBreeders 
                        |> Combinatorics.drawFromWeightedDistribution 
                                sorterFitnessEx
                                randy
                        |> Seq.take (SorterCount.value poolCount - stsWinners.Length) 
                        |> Seq.map(fun w-> rewrap w (mutator (sorterEx w)))
                        |> Seq.toArray
        stsWinners |> Array.append stsMutants


    let getSortingResults (sortableSet:SortableSet) (sorters:Sorter[]) = 
        SorterOps.StopIfSorted sortableSet sorters true


    let SuccessfulSorterFitness (fitf:FitnessFunc) (sorterRes:StandardSorterTestResults) =
        fitf.func sorterRes


    let checkArray (a:'a[]) =
        if a.Length < 1 then
            Console.WriteLine ("Array is empty")
            false
        else true


    let RunSorterMpG (sorterInfo:string) 
                     (sorter:Sorter)
                     (prams:PoolUpdateParams) 
                     (reportingFrequency:ReportingFrequency)
                     logToFile =
        let mutator = Sorter.mutate prams.mutationType

        let reportEvalBinsMin (genPool:int) (results:(Sorter*StandardSorterTestResults*SorterFitness)[]) =
            let summary = results|> Array.minBy (fun (srtr,r,f)-> r.switchUseCount)
            let (srtr,r,f) = summary
            sprintf "%s %d %d %.3f %.3f %s %d %d %d" sorterInfo (SwitchCount.value r.switchUseCount) (StageCount.value r.stageUseCount)
                     (PoolFraction.value prams.breederFrac) (PoolFraction.value prams.winnerFrac)
                     (MutationTypeF.StrF prams.mutationType)
                     (SorterCount.value prams.poolCount) genPool (RandomSeed.value prams.rngGen.seed)

        let reWrap (wrp:(Sorter*SwitchCount*StageCount)*SorterFitness) (srtr:Sorter) =
            let (s, w, t), f = wrp
            ((srtr, w, t), f)
        
        let randoLcgV = Rando.fromRngGen prams.rngGen

        let nextGenArgs sorterWraps =
            NextGen<Sorter*StandardSorterTestResults*SorterFitness, Sorter>
                    (mutator randoLcgV) prams.poolCount prams.breederFrac 
                     prams.winnerFrac randoLcgV
                       (fun (srtr,r,f) -> srtr) 
                       (fun (srtr,r,f) -> f)
                       (fun wrap srtr -> srtr)
                       sorterWraps

        let sortableSet = SortableSet.allBinary sorter.degree |> Result.ExtractOrThrow

        let sortersGen0 = seq {1.. (SorterCount.value prams.poolCount)}
                                |> Seq.map(fun _ -> mutator randoLcgV sorter)
                                |> Seq.toArray

        Console.WriteLine(sprintf "%s %d" sorterInfo (RandomSeed.value prams.rngGen.seed))

        let mutable currentEvals = Array.copy(sortersGen0)
        let mutable gen = 0
        let mutable nextRep = 0
        while (gen < (GenerationNumber.value prams.generationNumber)) && (checkArray currentEvals) do
            let genN = GenerationNumber.fromInt gen
            let currentSorterFitness = currentEvals |> (getSortingResults sortableSet)
                                       |> Seq.map(fun r -> (fst r), (snd r), (prams.fitnessFunc.func (snd r) genN))
                                       |> Seq.toArray
            if (nextRep = (ReportingFrequency.value reportingFrequency)) then 
                let binRec = currentSorterFitness |> reportEvalBinsMin (gen * (SorterCount.value prams.poolCount))
                logToFile binRec true
                nextRep <- 0

            currentEvals <- nextGenArgs currentSorterFitness
            gen <- gen + 1
            nextRep <- nextRep + (SorterCount.value prams.poolCount)

        if (checkArray currentEvals) then
            let genN = GenerationNumber.fromInt gen
            let binRecords = currentEvals 
                              |> getSortingResults sortableSet
                              |> Seq.map(fun r -> (fst r), (snd r), (prams.fitnessFunc.func (snd r) genN))
                              |> Seq.toArray |> reportEvalBinsMin (gen * (SorterCount.value prams.poolCount))
            logToFile binRecords true
        true


    let RunSorterMpgBatch (logfile:string)
                          (reportingFrequency:int)
                          (paramSeed:int) 
                          (sorterSeed:int)
                          (degree:int)
                          (switchOrStage:string)
                          (initialConditionCount:int)
                          (replicaCount:int)
                          (poolTimesGenCount:int) =
        let LogToFile =
            LogUtils.logFile logfile
        LogToFile "starting RunSorterMpgBatch" false
        let repFreq = ReportingFrequency.create "" reportingFrequency |> Result.ExtractOrThrow
        let icCount = InitialConditionCount.create "" initialConditionCount |> Result.ExtractOrThrow
        let poolGenCount = PoolGenCount.create "" poolTimesGenCount |> Result.ExtractOrThrow

        let replicaCount = ReplicaCount.create "" replicaCount |> Result.ExtractOrThrow
        let degree = Degree.create "" degree |> Result.ExtractOrThrow
        let sorterLength = SorterLength.to999Sucessful degree switchOrStage |> Result.ExtractOrThrow
        let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow

        let paramRndGen = RngGen.createLcg paramSeed
        let sorterRando = Rando.LcgFromSeed sorterSeed

        let paramReport =
            sprintf "degree:%d sorterSeed:%d paramSeed:%d sorterLength:%s replicaCount:%d" 
                    (Degree.value degree) sorterSeed paramSeed
                    (sorterLength |> SorterLengthDto.toString)
                    (ReplicaCount.value replicaCount)

        LogToFile paramReport true
        let reportHeader = "id sw1 st1 sw2 st2 bFrac wFrac mutTy poolCt genPool seed"
        LogToFile reportHeader true

        let RunSorterMpgParams (q:string*Sorter) (prams:PoolUpdateParams) =
            let (info,sorter) = q
            RunSorterMpG info sorter  prams repFreq LogToFile
        
        let sorterInfo w t = sprintf "%s %d %d" (string (Guid.NewGuid())) 
                                     (SwitchCount.value w) (StageCount.value t)

        let sorterCount = SorterCount.create "" (InitialConditionCount.value icCount) |> Result.ExtractOrThrow
        let sorterEvals = (SorterSet.createRandom degree sorterLength None sorterCount sorterRando).sorters
                            |> (getSortingResults sortableSet)
                            |> Seq.map(fun (srtr,r) -> (sorterInfo r.switchUseCount r.stageUseCount), srtr)
                            |> Seq.toArray

        let sorterAndPrams = PoolUpdateParams.ParamsM paramRndGen poolGenCount
                                |> Seq.take (ReplicaCount.value replicaCount)
                                |> Seq.toArray
                                |> Array.allPairs sorterEvals   
                                    
        let _res = sorterAndPrams |> Array.map(fun q -> RunSorterMpgParams (fst q) (snd q))

        "RunSorterMpBatch is done"