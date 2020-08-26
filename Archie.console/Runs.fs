namespace Archie.console
open Archie.Base
open System
open Archie.Base.SorterParts


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


    let SuccessfulSortResults (sortableSet:SortableSet) (sorters:Sorter[]) = 
        let res = SorterOps.StopIfSorted sortableSet sorters true
        res |> Seq.filter(fun (_, _, r) -> r=sortableSet.count)
            |> Seq.map(fun (s, u, _) -> s,u)


    let SuccessfulSorterEvals (sorterRes:seq<Sorter*SwitchUses>) = 
        sorterRes
            |> Seq.map(fun (s, u) -> SwitchUses.getStats s u)
            |> Seq.filter(fun r -> r |> Result.isOk)
            |> Seq.map(fun r -> r |> Result.ExtractOrThrow)

    let SuccessfulSorterFitness (sorterRes:seq<Sorter*SwitchCount*StageCount>) = 
        let ff w = SorterGa.sorterFitness 4.0 (SwitchCount.value w)
        sorterRes|> Seq.map(fun (s,w,t) -> ((s,w,t), ff w))

    //let SuccessfulSorterFitness (sorterRes:seq<Sorter*SwitchCount*StageCount>) = 
    //    let ff w = SorterSetEval.fitnessInt 4.0 (SwitchCount.value w)
    //    sorterRes|> Seq.map(fun (s,w,t) -> ((s,w,t), ff w))
    

    let RunSorterMp (id:Guid) (sorter:Sorter) (switchCount:SwitchCount) (stageCount:StageCount) 
                    (mutationRate:MutationRate) (randomSeed:RandomSeed) (poolCount:SorterCount)
                    logToFile =
        let reportEvalBins (results: (Sorter*SwitchCount*StageCount)[]) =
            let summary = results|> Array.groupBy(fun (_,w,t) -> (w,t))
            Utils.printArrayf 
               (fun ((w,t), b) -> sprintf "%s %d %d %d %d %d" (string id) (SwitchCount.value switchCount) 
                                    (StageCount.value stageCount) (SwitchCount.value w) (StageCount.value t)
                                    (b|>Array.length))
               summary

        let randoLcgV = new RandomLcg(randomSeed) :> IRando
        let degree = Degree.create "" 16 |> Result.ExtractOrThrow
        let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow
        let mutator sorter = sorter |> Sorter.mutateBySwitch mutationRate randoLcgV

        let sorterEvals = seq {1.. (SorterCount.value poolCount)}
                                |> Seq.map(fun _ -> mutator sorter)
                                |> Seq.toArray
                                |> SuccessfulSortResults sortableSet |> SuccessfulSorterEvals
                                |> Seq.toArray

        let binRecords = sorterEvals |> reportEvalBins
        logToFile binRecords true
        true


    let RunSorterMpG (sorterInfo:string) (sorter:Sorter)
                     (mutator:IRando->Sorter->Sorter) (randomSeed:RandomSeed) (poolCount:SorterCount)
                     (breederCount:PoolFraction) (winnerCount:PoolFraction) (generationCount:GenerationCount)
                     logToFile =
        let reportEvalBins (gen:int) (results: ((Sorter*SwitchCount*StageCount)*SorterFitness)[])  =
            let summary = results|> Array.groupBy(fun ((_,w,t), f) -> (w,t))
            Utils.printArrayf 
               (fun ((w,t), b) -> sprintf "%s %d %d %d %d %d" sorterInfo (SwitchCount.value w) (StageCount.value t)
                                           (b|>Array.length) gen (RandomSeed.value randomSeed))
               summary

        let reportEvalBinsMin (gen:int) (results: ((Sorter*SwitchCount*StageCount)*SorterFitness)[])  =
            let summary = results|> Array.minBy (fun ((_,w,t), f) -> w)
            let ((s,w,t), b) = summary
            sprintf "%s %d %d %d %d" sorterInfo (SwitchCount.value w) (StageCount.value t)
                     gen (RandomSeed.value randomSeed)

        let reWrap (wrp:(Sorter*SwitchCount*StageCount)*SorterFitness) (srtr:Sorter) =
            let (s, w, t), f = wrp
            ((srtr, w, t), f)
        
        let randoLcgV = new RandomLcg(randomSeed) :> IRando
        let nextGenArgs sorterWraps =
            NextGen<(Sorter*SwitchCount*StageCount)*SorterFitness, Sorter>
                    (mutator randoLcgV) poolCount breederCount winnerCount randoLcgV
                       (fun ((s,w,t), f) -> s) 
                       (fun ((s,w,t), f) -> f)
                       (fun wrap srtr -> srtr)
                       sorterWraps

        let sortableSet = SortableSet.allBinary sorter.degree |> Result.ExtractOrThrow

        let sortersGen0 = seq {1.. (SorterCount.value poolCount)}
                                |> Seq.map(fun _ -> mutator randoLcgV sorter)
                                |> Seq.toArray

        Console.WriteLine((RandomSeed.value randomSeed))

        let mutable currentEvals = Array.copy(sortersGen0)
        let mutable i = 0
        while i < (GenerationCount.value generationCount) - 1 do
            let currentSorterFitness = currentEvals |> (SuccessfulSortResults sortableSet)
                                       |> SuccessfulSorterEvals |> SuccessfulSorterFitness
                                       |> Seq.toArray
            let binRec = currentSorterFitness |> reportEvalBinsMin i
            logToFile binRec true
            currentEvals <- nextGenArgs currentSorterFitness
            i <- i + 1
            
        let binRecords = currentEvals |> SuccessfulSortResults sortableSet
                                      |> SuccessfulSorterEvals |> SuccessfulSorterFitness
                                      |> Seq.toArray |> reportEvalBinsMin i
        logToFile binRecords true
        true


    let RunSorterMpgBatch (logfile:string) =
        let LogToFile = 
            Utils.logFile logfile
        LogToFile "starting RunSorterMpgBatch" false

        let sorterCount = SorterCount.create "" 100 |> Result.ExtractOrThrow
        let replicaCount = ReplicaCount.create "" 100 |> Result.ExtractOrThrow
        let degree = Degree.create "" 10 |> Result.ExtractOrThrow
        let rndSeed = Rando.GetSeed |> Result.ExtractOrThrow
        let randoLcg = new RandomLcg(rndSeed) :> IRando
        let stageCount = StageCount.create "" 40 |> Result.ExtractOrThrow
        let randSorterGen = RandSorterGeneration.Stage stageCount
        let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow
        let poolCount = SorterCount.create "" 25 |> Result.ExtractOrThrow
        let breederFrac = PoolFraction.create "" 0.40 |> Result.ExtractOrThrow
        let winnerFrac = PoolFraction.create "" 0.20 |> Result.ExtractOrThrow
        let genCount = (GenerationCount.create "" 200) |> Result.ExtractOrThrow
        
        let mutationRate = MutationRate.create "" 0.01 |> Result.ExtractOrThrow
        let mutationType = MutationType.Switch mutationRate

        let paramReport =
            sprintf "degree:%d randomSeed:%d stageCount:%d mutationRate:%f replicaCount:%d
                     poolCount:%d breederFrac:%.2f winnerFrac:%.2f genCount:%d" 
                (Degree.value degree) (RandomSeed.value rndSeed)
                (StageCount.value stageCount) (MutationRate.value mutationRate)
                (ReplicaCount.value replicaCount)
                (SorterCount.value poolCount) (PoolFraction.value breederFrac)
                (PoolFraction.value winnerFrac) (GenerationCount.value genCount)

        LogToFile paramReport true

        let RunSorterMpgParams (q:string*Sorter) (seed:int) =
            let (info,sorter) = q
            let randomSeed = RandomSeed.create "" seed |> Result.ExtractOrThrow
            let mutator = Sorter.mutateBySwitch mutationRate
            RunSorterMpG info sorter mutator randomSeed poolCount 
                         breederFrac winnerFrac genCount LogToFile
        
        let sorterInfo w t = sprintf "%s %d %d" (string (Guid.NewGuid())) 
                                     (SwitchCount.value w) (StageCount.value t)

        let sorterEvals = (SorterSet.createRandom degree randSorterGen sorterCount randoLcg).sorters
                            |> (SuccessfulSortResults sortableSet)
                            |> SuccessfulSorterEvals 
                            |> Seq.map(fun (s, w, t) -> (sorterInfo w t), s)
                            |> Seq.toArray

        let sortersSeeds = seq {1 .. (ReplicaCount.value replicaCount)}
                        |> Seq.map(fun _ -> randoLcg.NextPositiveInt)
                        |> Seq.toArray
                        |> Array.allPairs sorterEvals

        let _res = sortersSeeds |> Array.map(fun q -> RunSorterMpgParams (fst q) (snd q))

        "RunSorterMpBatch is done"