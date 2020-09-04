namespace Archie.console
open Archie.Base
open System


module Runs = 

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
        |> Seq.map(fun (s, u, r) -> let w, t = (SwitchUses.getSwitchAndStageUses s u)
                                    s, u, r, w, t)


    let SuccessfulSorterFitness (sorterRes:seq<Sorter*SwitchUses*SortableCount*SwitchCount*StageCount>) = 
        let ff w = (FitnessFunc.standardSwitch 4.0).fitnessFunc ((SwitchCount.value w) :> obj)
        sorterRes|> Seq.map(fun (srtr,su,ss,w,t) -> (srtr,su,ss,w,t, ff w))

    //let SuccessfulSorterFitness (sorterRes:seq<Sorter*SwitchCount*StageCount>) = 
    //    let ff w = SorterSetEval.fitnessInt 4.0 (SwitchCount.value w)
    //    sorterRes|> Seq.map(fun (s,w,t) -> ((s,w,t), ff w))
    
    let checkArray (a:'a[]) =
        if a.Length < 1 then
            Console.WriteLine ("Array is empty")
            false
        else true

    let RunSorterMpG (sorterInfo:string) 
                     (sorter:Sorter)
                     (prams:PoolUpdateParams)
                     logToFile =

        let mutator = Sorter.mutate prams.mutationType

        let reportEvalBinsMin (genPool:int) (results:(Sorter*SwitchUses*SortableCount*SwitchCount*StageCount*SorterFitness)[]) =
            let summary = results|> Array.minBy (fun (srtr,su,ss,w,t,f)-> w)
            let (srtr,su,ss,w,t,f) = summary
            sprintf "%s %d %d %.3f %.3f %s %d %d %d" sorterInfo (SwitchCount.value w) (StageCount.value t)
                     (PoolFraction.value prams.breederFrac) (PoolFraction.value prams.winnerFrac)
                     (MutationTypeF.StrF prams.mutationType)
                     (SorterCount.value prams.poolCount) genPool (RandomSeed.value prams.rngGen.seed)

        let reWrap (wrp:(Sorter*SwitchCount*StageCount)*SorterFitness) (srtr:Sorter) =
            let (s, w, t), f = wrp
            ((srtr, w, t), f)
        
        let randoLcgV = Rando.fromRngGen prams.rngGen

        let nextGenArgs sorterWraps =
            NextGen<Sorter*SwitchUses*SortableCount*SwitchCount*StageCount*SorterFitness, Sorter>
                    (mutator randoLcgV) prams.poolCount prams.breederFrac prams.winnerFrac randoLcgV
                       (fun (srtr,su,ss,w,t,f) -> srtr) 
                       (fun (srtr,su,ss,w,t,f) -> f)
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
        while (gen < (GenerationCount.value prams.generationCount)) && (checkArray currentEvals) do
            let currentSorterFitness = currentEvals 
                                       |> (getSortingResults sortableSet)
                                       |> SuccessfulSorterFitness
                                       |> Seq.toArray
            if (nextRep = 128) then 
                let binRec = currentSorterFitness |> reportEvalBinsMin (gen * (SorterCount.value prams.poolCount))
                logToFile binRec true
                nextRep <- 0

            currentEvals <- nextGenArgs currentSorterFitness
            gen <- gen + 1
            nextRep <- nextRep + (SorterCount.value prams.poolCount)

        if (checkArray currentEvals) then
            let binRecords = currentEvals 
                                |> getSortingResults sortableSet
                                |> SuccessfulSorterFitness
                                |> Seq.toArray |> reportEvalBinsMin (gen * (SorterCount.value prams.poolCount))
            logToFile binRecords true
        true


    let RunSorterMpgBatch (logfile:string) 
                          (degree:int) 
                          (sorterCount:int) 
                          (replicaCount:int)
                          (sorterGenSeed:int)
                          (switchOrStage:string)
                          (replicaGenSeed:int) 
                          (poolCount:int) =
        let LogToFile =         
            LogUtils.logFile logfile
        LogToFile "starting RunSorterMpgBatch" false

        let d = Degree.create "" 14 |> Result.ExtractOrThrow
        let sorterCount = SorterCount.create "" sorterCount |> Result.ExtractOrThrow
        let rsg = RngGen.createLcg sorterGenSeed
        let rspp = RndSorterParams.Make d sorterCount rsg switchOrStage |> Result.ExtractOrThrow

        let rc = ReplicaCount.create "" replicaCount |> Result.ExtractOrThrow
        let sortableSet = SortableSet.allBinary d |> Result.ExtractOrThrow



        None


    //    let paramRndGen = RngGenF.createLcg paramSeed

    //    let paramReport = sprintf "degree:%d sorterCount:%d replicaCount:%d 
    //                               sorterSeed:%d switchOrStage:%s paramSeed:%d startingConditionCount:%d"
    //                               degree sorterCount replicaCount sorterSeed switchOrStage
    //                               paramSeed startingConditionCount

    //    LogToFile paramReport true
    //    let reportHeader = "id sw1 st1 sw2 st2 bFrac wFrac mutTy poolCt genPool seed"
    //    LogToFile reportHeader true

    //    let RunSorterMpgParams (q:string*Sorter) (prams:PoolUpdateParams) =
    //        let (info,sorter) = q
    //        RunSorterMpG info sorter prams LogToFile

        
    //    let sorterInfo w t = sprintf "%s %d %d" (string (Guid.NewGuid())) 
    //                                 (SwitchCount.value w) (StageCount.value t)

    //    let sorterEvals = (SorterSet.createRandom degree randSorterGen sorterCount sorterRando).sorters
    //                        |> (SuccessfulSortResults sortableSet)
    //                        |> SuccessfulSorterEvals 
    //                        |> Seq.map(fun (s, w, t) -> (sorterInfo w t), s)
    //                        |> Seq.toArray

    //    let sorterAndPrams = PoolUpdateParams.ParamsM paramRndGen startingConditionCount
    //                            |> Seq.take (ReplicaCount.value replicaCount)
    //                            |> Seq.toArray
    //                            |> Array.allPairs sorterEvals   
                                    
    //    let _res = sorterAndPrams |> Array.map(fun q -> RunSorterMpgParams (fst q) (snd q))

    //    "RunSorterMpBatch is done"