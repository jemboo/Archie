namespace Archie.Base
open System

type SorterPoolUpdateParams = 
  {
      id: Guid;
      breederSelector: PoolSelector2;
      fitnessFunc: FitnessFunc;
      sorterMutator: SorterMutation;
      sorterCount: SorterCount;
      winnerSelector: PoolSelector2;
  }


module SorterPoolUpdateParams =

    let ss (fitnessFunc:FitnessFunc)
           (breederFrac:PoolFraction)
           (mutationType:SorterMutationType) 
           (sorterCount:SorterCount)
           (winnerFrac:PoolFraction) =





























































































































        let poolId = GuidUtils.guidFromObjs(
                        seq { (fitnessFunc :> obj);
                              (breederFrac :> obj);
                              (mutationType :> obj);
                              (sorterCount :> obj);
                              (winnerFrac :> obj); } )
        {
            SorterPoolUpdateParams.id = poolId;
            breederSelector = breederFrac |> PoolSelector2.standard;
            fitnessFunc = fitnessFunc;
            sorterMutator = SorterMutation.standardMutator mutationType;
            sorterCount = sorterCount;
            winnerSelector = winnerFrac |> PoolSelector2.standard;
        }

type SorterPoolState =
    | Initiated
    | Measured
    | Evaluated
    | Selected

module SorterPoolState =
    let IsCompatable (sorterPoolState:SorterPoolState) 
                     (poolMemberState:PoolMemberState) =
        match sorterPoolState with
        | Initiated -> (poolMemberState =  PoolMemberState.Root) || 
                        (poolMemberState = PoolMemberState.Initiate) || 
                        (poolMemberState = PoolMemberState.Legacy)
        | Measured -> (poolMemberState = PoolMemberState.Measured)
        | Evaluated -> (poolMemberState = PoolMemberState.Evaluated)
        | Selected -> (poolMemberState = PoolMemberState.Evaluated)


type SorterPool2 =
  {
        id: Guid;
        degree:Degree;
        sorterPoolMembers: SorterPoolMember list;
        sorterPoolState: SorterPoolState;
        generation:GenerationNumber
  }

module SorterPool2 =

    let create (id:Guid) 
               (degree:Degree) 
               (sorterPoolMembers:SorterPoolMember list)
               (generation:GenerationNumber) 
               (sorterPoolState:SorterPoolState) =
        let dupes = sorterPoolMembers 
                    |> List.map(fun m->m.id)
                    |> CollectionUtils.duplicates
                    |> Seq.toArray

        let wrongDegree = sorterPoolMembers 
                            |> List.filter(fun m -> (m.sorter.degree <> degree))

        let matchingStates = 
            sorterPoolMembers 
                |> List.forall(fun spm -> SorterPoolState.IsCompatable 
                                                sorterPoolState spm.poolMemberState)
                           
        if (dupes.Length > 0) then
            "duplicates found in list" |> Error
        elif (wrongDegree.Length > 0) then
            "mixed degrees found in list" |> Error
        elif (not matchingStates) then
            sprintf "not all member states are compatable with %s" (sorterPoolState.ToString()) |> Error
        else
            {
                id=id;
                degree=degree;
                sorterPoolMembers=sorterPoolMembers;
                generation=generation
                sorterPoolState=sorterPoolState
            } |> Ok
    
    let createRandom (degree:Degree) 
                     (sorterLength:SorterLength) 
                     (switchFreq:SwitchFrequency)
                     (sorterCount:SorterCount) 
                     (rngSorters:RngGen)
                     (rngTypeForIds:RngType) =

        let poolId = GuidUtils.guidFromObjs(
                      seq { (degree :> obj); 
                            (sorterLength :> obj); 
                            (switchFreq :> obj); 
                            (sorterCount :> obj);
                            (rngSorters :> obj);} )

        let randyForIds = Rando.fromGuid rngTypeForIds poolId
        let ids = seq {1 .. (SorterCount.value sorterCount)}
                  |> Seq.map(fun _ ->  Rando.NextGuid randyForIds None)
                  |> Seq.toList

        let rndSorters = Rando.fromRngGen rngSorters
        let sorterPoolMembers = ids |> List.map(fun g -> 
                SorterPoolMember.makeRoot 
                    g
                    (Sorter.createRandom degree sorterLength (Some switchFreq) rndSorters)
                    None 
                    None)
        create poolId degree sorterPoolMembers  (GenerationNumber.fromInt 0) SorterPoolState.Initiated
        

    let Measure (sortableSet:SortableSet) 
                (sorterPool:SorterPool2) =

        let eval = SorterOps.GetTheStandardSortingResultsComplete sortableSet
        let measure spm = 
                match spm.poolMemberState with
                        | Legacy -> spm |> Ok
                        | _ -> SorterPoolMember.toMeasured spm eval

        if sorterPool.sorterPoolState <> SorterPoolState.Initiated then
            sprintf "SorterPoolState %s is not for Measure" 
                        (sorterPool.sorterPoolState.ToString()) |> Result.Error
        else
            result {
                let! spRet = sorterPool.sorterPoolMembers
                            |> List.map(measure)
                            |> Result.sequence
            return 
                {
                     SorterPool2.id = sorterPool.id;
                     degree=sorterPool.degree;
                     sorterPoolMembers=spRet;
                     sorterPoolState=SorterPoolState.Measured;
                     generation=sorterPool.generation
                }
            }


    let Evaluate (prams:SorterPoolUpdateParams) 
                 (fitnessFuncParam:FitnessFuncParam)
                 (sorterPool:SorterPool2) =

        let evalo (spm:SorterPoolMember) =
            match spm.poolMemberState with
                    | Legacy -> spm |> Ok
                    | _ -> SorterPoolMember.toEvaluated spm prams.fitnessFunc fitnessFuncParam

        if sorterPool.sorterPoolState <> SorterPoolState.Measured then
            sprintf "SorterPoolState %s is not for Evaluate" 
                    (sorterPool.sorterPoolState.ToString()) |> Result.Error
        else
            result {
                let! spRet = sorterPool.sorterPoolMembers
                            |> List.map(evalo)
                            |> Result.sequence
            return 
                {
                     SorterPool2.id = sorterPool.id;
                     degree=sorterPool.degree;
                     sorterPoolMembers=spRet;
                     sorterPoolState=SorterPoolState.Evaluated;
                     generation=sorterPool.generation
                }
            }

        
    let Select (prams:SorterPoolUpdateParams) 
               (fitnessFuncParam:FitnessFuncParam)
               (sorterPool:SorterPool2) =

        //let breederCount = PoolFraction.boundedMultiply prams.breederFrac sorterPool.Length
        //let winnerCount = PoolFraction.boundedMultiply prams.winnerFrac sorterPool.Length
        //let mutantCount = SorterCount.value prams.poolCount - winnerCount

        //let fitnessRankedMembers =
        //    sorterPool
        //    |> Array.map(fun w ->
        //        (w, (SorterFitness.value (w.fitness |> Option.get))))
        //    |> Array.sortByDescending(snd)

        //let stsBreeders = fitnessRankedMembers |> Array.take(breederCount)

        let yak = PS.value prams.winnerSelector.func
        let nubes = sorterPool.sorterPoolMembers 
                    |> List.toArray 
                    |> yak 












        let evalo (spm:SorterPoolMember) =
            match spm.poolMemberState with
                    | Legacy -> spm |> Ok
                    | _ -> SorterPoolMember.toEvaluated spm prams.fitnessFunc fitnessFuncParam

        if sorterPool.sorterPoolState <> SorterPoolState.Evaluated then
            sprintf "SorterPoolState %s is not for Select" 
                    (sorterPool.sorterPoolState.ToString()) |> Result.Error
        else
            result {
                let! spRet = sorterPool.sorterPoolMembers
                            |> List.map(evalo)
                            |> Result.sequence
            return 
                {
                        SorterPool2.id = sorterPool.id;
                        degree=sorterPool.degree;
                        sorterPoolMembers=spRet;
                        sorterPoolState=SorterPoolState.Evaluated;
                        generation=sorterPool.generation
                }
            }


    let Reproduce (prams:SorterPoolUpdateParams) 
                  (fitnessFuncParam:FitnessFuncParam)
                  (sorterPool:SorterPool2) =















        let evalo (spm:SorterPoolMember) =
            match spm.poolMemberState with
                    | Legacy -> spm |> Ok
                    | _ -> SorterPoolMember.toEvaluated spm prams.fitnessFunc fitnessFuncParam

        if sorterPool.sorterPoolState <> SorterPoolState.Selected then
            sprintf "SorterPoolState %s is not for Reproduce" 
                    (sorterPool.sorterPoolState.ToString()) |> Result.Error
        else
            result {
                let! spRet = sorterPool.sorterPoolMembers
                            |> List.map(evalo)
                            |> Result.sequence
            return 
                {
                        SorterPool2.id = sorterPool.id;
                        degree=sorterPool.degree;
                        sorterPoolMembers=spRet;
                        sorterPoolState=SorterPoolState.Evaluated;
                        generation=sorterPool.generation
                }
            }








type SorterPoolRunParams = 
  {
     id:Guid;
     startingSorterPool:SorterPool2;
     runLength:GenerationNumber;
     sorterPoolUpdateParams:SorterPoolUpdateParams;
     rngGens:Map<string, RngGen>
  }


module SorterPoolRunParams =

    let ss (startingSorterPool:SorterPool2)
           (rngGenMut:RngGen)
           (runLength:GenerationNumber)
           (sorterPoolUpdateParams:SorterPoolUpdateParams) =

        let runlId = GuidUtils.guidFromObjs(
                        seq { (startingSorterPool.id :> obj);
                              (rngGenMut :> obj);
                              (sorterPoolUpdateParams.id :> obj);} )
        {
            SorterPoolRunParams.id = runlId;
            startingSorterPool = startingSorterPool;
            runLength = runLength;
            sorterPoolUpdateParams = sorterPoolUpdateParams;
            rngGens = ([| ( "sorterMut", rngGenMut) |] |> Map.ofArray)
        }


type SorterPoolBatchRunParams = {
   id:Guid;
   sorterPoolRunParamsSet:SorterPoolRunParams[];
}


module SorterPoolBatchRunParams =

    let private fromGens   (id:Guid)
                           (rngGenMut:RngGen)
                           (startingSorterPoolGen:int->SorterPool2)
                           (runLengthGen:int->GenerationNumber)
                           (sorterPoolUpdateParamsGen:int->SorterPoolUpdateParams)
                           (runParamsCount:RunCount) =

        let makeSorterPoolRunParams dex rngGenMut = 
            SorterPoolRunParams.ss (startingSorterPoolGen dex)
                                   rngGenMut
                                   (runLengthGen dex)
                                   (sorterPoolUpdateParamsGen dex)

        let sorterPoolRunParamsSet = RandoCollections.IndexedSeedGen rngGenMut
                                      |> Seq.map(fun (dex, rng) -> makeSorterPoolRunParams dex rng)
                                      |> Seq.take (RunCount.value runParamsCount)
                                      |> Seq.toArray
        {
            id=id;
            sorterPoolRunParamsSet = sorterPoolRunParamsSet
        }

    let makeBatchOfRunReplicas
                        (rngSorters:RngGen)
                        (rngTypeForSorterIds:RngType)
                        (degree:Degree)
                        (sorterLength:SorterLength)
                        (switchFreq:SwitchFrequency)
                        (sorterPoolSize:SorterCount)
                        (rngGenMut:RngGen)
                        (breederFrac:PoolFraction)
                        (winnerFrac:PoolFraction)
                        (sorterMutationType:SorterMutationType)
                        (runLength:GenerationNumber) 
                        (poolCount:PoolCount)
                        (runCount:RunCount) =

              let batchRunlId = GuidUtils.guidFromObjs(
                                  seq { (rngSorters :> obj);
                                        (rngTypeForSorterIds :> obj);
                                        (degree :> obj);
                                        (sorterLength :> obj);
                                        (switchFreq :> obj);
                                        (sorterPoolSize :> obj);
                                        (rngGenMut :> obj);
                                        (breederFrac :> obj);
                                        (winnerFrac :> obj);
                                        (sorterMutationType :> obj);
                                        (runLength :> obj);
                                        (poolCount :> obj);} )

              let makeStartingSorterPool rngGenSorter = 
                  SorterPool2.createRandom degree sorterLength switchFreq
                                           sorterPoolSize rngGenSorter rngTypeForSorterIds

              let startingSorterPoolGen (sorterPools:SorterPool2 list) (dex:int)  =
                  sorterPools.[dex % (sorterPools.Length)]

              let runLengthGen dex =
                  runLength

              let sorterPoolUpdateParamsGen (dex:int) = 
                  SorterPoolUpdateParams.ss
                        FitnessFunc.standardStage 
                        breederFrac sorterMutationType sorterPoolSize winnerFrac

              result {
                  let! pools = RandoCollections.IndexedSeedGen rngSorters
                                |> Seq.map(fun (dex, rng) -> makeStartingSorterPool rng)
                                |> Seq.take (PoolCount.value poolCount)
                                |> Seq.toList
                                |> Result.sequence 

                  return fromGens batchRunlId rngGenMut (startingSorterPoolGen pools)
                                  runLengthGen sorterPoolUpdateParamsGen runCount
              }