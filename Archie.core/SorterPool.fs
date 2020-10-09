namespace Archie.Base
open System

type SorterPoolUpdateParams = 
  {
      id: Guid;
      breederSelector: SorterPoolMember[] -> seq<SorterPoolMember>;
      fitnessFunc: FitnessFunc;
      sorterMutator: SorterMutation;
      sorterCount: SorterCount;
      winnerSelector: SorterPoolMember[] -> seq<SorterPoolMember>;
  }

type SorterPool2 = 
  {
        id: Guid;
        degree:Degree;
        sorterPoolMembers: SorterPoolMember[];
  }

module SorterPool2 =
    
    let create (id:Guid) (degree:Degree) 
               (sorterPoolMembers:SorterPoolMember[]) =
        let dupes = sorterPoolMembers 
                    |> Array.map(fun m->m.id)
                    |> CollectionUtils.duplicates
                    |> Seq.toArray

        let wrongDegree = sorterPoolMembers 
                            |> Array.filter(fun m -> (m.sorter.degree <> degree))

        if (dupes.Length > 0) then
            "duplicates found in list" |> Error
        elif (wrongDegree.Length > 0) then
            "mixed degrees found in list" |> Error
        else
            {
                id=id;
                degree=degree;
                sorterPoolMembers=sorterPoolMembers;
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
                  |> Seq.toArray

        let rndSorters = Rando.fromRngGen rngSorters
        let sorterPoolMembers = ids |> Array.map(fun g -> 
                SorterPoolMember.makeRoot 
                    g
                    (Sorter.createRandom degree sorterLength (Some switchFreq) rndSorters)
                    None 
                    None)
        create poolId degree sorterPoolMembers
        


type SorterPoolRunParams = 
  {
     id:Guid;
     startingSorterPool:SorterPool2;
     runLength:GenerationNumber;
     sorterPoolUpdateParams:SorterPoolUpdateParams;
     rngGens:Map<string, RngGen>
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
                              (winnerFrac :> obj);} )

        let breederCount = PoolFraction.boundedMultiply breederFrac (SorterCount.value sorterCount)
        let winnerCount = PoolFraction.boundedMultiply winnerFrac (SorterCount.value sorterCount)
        let breederSelector (sorterPoolMembers:SorterPoolMember[]) =
            sorterPoolMembers
            |> Array.sortByDescending(fun spm-> SorterPoolMember.getFitness spm)
            |> Seq.take breederCount

        let winnerSelector (sorterPoolMembers:SorterPoolMember[]) =
            sorterPoolMembers
            |> Array.sortByDescending(fun spm-> SorterPoolMember.getFitness spm)
            |> Seq.take winnerCount

        {
            SorterPoolUpdateParams.id = poolId;
            breederSelector = breederSelector;
            fitnessFunc = fitnessFunc;
            sorterMutator = SorterMutation.standardMutator mutationType;
            sorterCount = sorterCount;
            winnerSelector = winnerSelector;
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

              

//let createRandom (degree:Degree) 
//                 (sorterLength:SorterLength) 
//                 (switchFreq:float)
//                 (sorterCount:SorterCount) 
//                 (rngSorters:RngGen)
//                 (rngTypeIds:RngType) =