﻿namespace Archie.Base
open System

module SorterPoolMemberF =

    let makeRoot id rootSorter res fit =
        {
            SorterPoolMember.id=id;
            poolMemberState=PoolMemberState.Root; 
            birthDate=GenerationNumber.fromInt 0; 
            parent=None;
            sorter=rootSorter;
            poolMemberRank=None;
            testResults=res;
            fitness=fit;
        }

    let toArchived (archiver:SorterPoolMember->unit) (spm:SorterPoolMember) =
        archiver spm
        match spm.poolMemberState with
        | Legacy | Evaluated | Initiate | Measured | Root  ->
        {
            id=spm.id;
            poolMemberState=PoolMemberState.Archived; 
            birthDate=spm.birthDate; 
            parent=None;
            sorter=spm.sorter;
            poolMemberRank=spm.poolMemberRank;
            testResults=spm.testResults;
            fitness=spm.fitness;
        }
        | Archived _ -> spm

        
    let toInitiate (mutator:Sorter->Sorter) (parent:SorterPoolMember) 
                   (generation:GenerationNumber) =
        { 
            SorterPoolMember.id = Guid.NewGuid();
            poolMemberState=PoolMemberState.Initiate;
            birthDate=generation;
            parent=Some parent;
            sorter= (mutator parent.sorter);
            poolMemberRank=None;
            testResults=None;
            fitness=None;
        }


    let toMeasured (pm:SorterPoolMember) (measure:Sorter->SorterTestResults) =
        match pm.poolMemberState with
        | Archived _ -> failwith "cannot convert Archived to Tested"

        | _ ->
            {SorterPoolMember.id = pm.id;
             poolMemberState = PoolMemberState.Measured;
             birthDate = pm.birthDate;
             parent = pm.parent;
             sorter = pm.sorter;
             poolMemberRank = None;
             testResults = Some (measure pm.sorter);
             fitness = None;}


    let toEvaluated (pm:SorterPoolMember) (fitnessFunc:FitnessFunc) 
                    (fitnessFuncParam:FitnessFuncParam) =
        match pm.poolMemberState with
        | Legacy | Measured | Evaluated ->
            {SorterPoolMember.id = pm.id;
             poolMemberState = PoolMemberState.Evaluated;
             birthDate = pm.birthDate;
             parent = pm.parent;
             sorter = pm.sorter;
             poolMemberRank = None;
             testResults = pm.testResults;
             fitness = Some ((fitnessFunc.func |> FF.value) fitnessFuncParam ((pm.testResults) |> Option.get));}

        | Root -> failwith "cannot convert Root to Evaluated"
        | Initiate -> failwith "cannot convert Initiate to Evaluated"
        | Archived -> failwith "cannot convert Archived to Evaluated"


    let toLegacy (pm:SorterPoolMember) (rank:PoolMemberRank) 
                 (archiver:SorterPoolMember->unit) =
        match pm.poolMemberState with
        | Legacy ->
            {SorterPoolMember.id = pm.id;
             poolMemberState=PoolMemberState.Legacy;
             birthDate=pm.birthDate;
             parent=pm.parent;
             sorter=pm.sorter;
             poolMemberRank=Some rank;
             testResults=pm.testResults;
             fitness=pm.fitness;}
        | Evaluated -> 
            {SorterPoolMember.id = pm.id;
             poolMemberState=PoolMemberState.Legacy;
             birthDate=pm.birthDate;
             parent= match pm.parent with
                     |Some pmem-> Some (pmem |> toArchived archiver);
                     |None -> None
             sorter=pm.sorter;
             poolMemberRank=Some rank;
             testResults=pm.testResults;
             fitness=pm.fitness;}
        | Measured -> failwith "cannot convert Measured to Legacy"
        | Initiate -> failwith "cannot convert Initiate to Legacy"
        | Archived -> failwith "cannot convert Archived to Legacy"
        | Root -> failwith "cannot convert Root to Legacy"


    let getAdjFitness (pm:SorterPoolMember) (fitness:SorterFitness) =
       match pm.poolMemberState with
       | Legacy -> SorterFitness.fromFloat(SorterFitness.value(pm.fitness |> Option.get) + SorterFitness.value(fitness))
       | Measured -> failwith "cannot getFitness from Initiate"
       | Evaluated -> pm.fitness |> Option.get
       | Initiate _ -> failwith "cannot getFitness from Initiate"
       | Archived _ -> failwith "cannot getFitness from Archived"
       | Root _ -> failwith "cannot getFitness from None"


    let getFitness (pm:SorterPoolMember) =
       match pm.poolMemberState with
       | Legacy | Evaluated -> pm.fitness |> Option.get
       | Measured -> failwith "cannot getFitness from Measured"
       | Initiate _ -> failwith "cannot getFitness from Initiate"
       | Archived _ -> failwith "cannot getFitness from Archived"
       | Root _ -> failwith "cannot getFitness from None"


    let isTopRanked (pm:SorterPoolMember) =
       match pm.poolMemberRank with
       | Some r -> r = (PoolMemberRank.fromInt 1)
       | None -> false


    let parentReportHeader = [|"parentId"; "parentBirthdate"; "parentRank"; "parentFitness";|]

    let reportParent (spm:SorterPoolMember) =
        match spm.parent with
        | Some p ->
                match p.poolMemberState with
                | Legacy | Evaluated | Initiate -> 
                    [|(sprintf "%A" p.id);
                    (sprintf "%d" (GenerationNumber.value (p.birthDate))); 
                    (sprintf "%s" (PoolMemberRank.repStr p.poolMemberRank));
                    (sprintf "%.4f" (SorterFitness.value (getFitness p)))|]
                | _ -> [|"";"";"";""|]
        | None -> [|"";"";"";""|]


    let reportHeader =  SorterTestResults.headers
                        |> Array.append parentReportHeader
                        |> Array.append [|"sorter_id"; "birthdate"; "rank"; "fitness"|]


    let report (spm:SorterPoolMember) =
          (SorterTestResults.reportOpt (spm.testResults))
          |> Array.append (reportParent (spm))
          |> Array.append [|(sprintf "%A" spm.id); 
                            (sprintf "%d" (GenerationNumber.value (spm.birthDate))); 
                            (sprintf "%s" (PoolMemberRank.repStr spm.poolMemberRank)); 
                            (sprintf "%s" (SorterFitness.repStr spm.fitness));|]


type SorterPoolUpdateParams = 
  {
      id: Guid;
      breederSelector: SorterPoolMember[] -> seq<SorterPoolMember>;
      fitnessFunc: FitnessFunc;
      sorterMutator: IRando->Sorter->Sorter;
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
                SorterPoolMemberF.makeRoot 
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
            |> Array.sortByDescending(fun spm-> SorterPoolMemberF.getFitness spm)
            |> Seq.take breederCount

        let winnerSelector (sorterPoolMembers:SorterPoolMember[]) =
            sorterPoolMembers
            |> Array.sortByDescending(fun spm-> SorterPoolMemberF.getFitness spm)
            |> Seq.take winnerCount

        {
            SorterPoolUpdateParams.id = poolId;
            breederSelector = breederSelector;
            fitnessFunc = fitnessFunc;
            sorterMutator = (Sorter.mutate mutationType);
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