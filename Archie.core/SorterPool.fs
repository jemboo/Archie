namespace Archie.Base
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

    let toArchived (spm:SorterPoolMember) =
        match spm.poolMemberState with
        | Legacy | Evaluated | Initiate | Measured ->
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
        | Root | Archived _ -> spm

        
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
        | Root -> 
            {SorterPoolMember.id = pm.id;
             poolMemberState = PoolMemberState.Measured;
             birthDate = pm.birthDate;
             parent = Some pm;
             sorter = pm.sorter;
             poolMemberRank = None;
             testResults = Some (measure pm.sorter);
             fitness = None;}
        | Initiate | Evaluated | Measured | Legacy ->
            {SorterPoolMember.id = pm.id;
             poolMemberState = PoolMemberState.Measured;
             birthDate = pm.birthDate;
             parent = pm.parent;
             sorter = pm.sorter;
             poolMemberRank = None;
             testResults = Some (measure pm.sorter);
             fitness = None;}
        | Archived _ -> failwith "cannot convert Archived to Tested"


    let toEvaluated (pm:SorterPoolMember) (fitnessFunc:FitnessFunc) 
                    (gen:GenerationNumber) =
        match pm.poolMemberState with
        | Legacy | Measured | Evaluated ->
            {SorterPoolMember.id = pm.id;
             poolMemberState = PoolMemberState.Evaluated;
             birthDate = pm.birthDate;
             parent = pm.parent;
             sorter = pm.sorter;
             poolMemberRank = None;
             testResults = pm.testResults;
             fitness = Some (fitnessFunc.func (Some (gen:>obj)) ((pm.testResults) |> Option.get));}
        | Root -> failwith "cannot convert Root to Evaluated"
        | Initiate -> failwith "cannot convert Initiate to Evaluated"
        | Archived -> failwith "cannot convert Archived to Evaluated"


    let toLegacy (pm:SorterPoolMember) (rank:PoolMemberRank) =
        match pm.poolMemberState with
        | Legacy | Measured | Evaluated -> 
            {SorterPoolMember.id = pm.id;
             poolMemberState=PoolMemberState.Legacy;
             birthDate=pm.birthDate;
             parent=pm.parent;
             sorter=pm.sorter;
             poolMemberRank=Some rank;
             testResults=pm.testResults;
             fitness=pm.fitness;}
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
      fitnessFunc: FitnessFunc2;
      sorterMutator: IRando->Sorter->Sorter;
      poolCount: SorterCount;
      winnerSelector: SorterPoolMember[] -> seq<SorterPoolMember>;
  }

type SorterPool2 = 
  {
        id: Guid;
        degree:Degree;
        sorterPoolMembers: SorterPoolMember[];
  }

module SorterPool2 =
    let createRandom (degree:Degree) 
                     (sorterLength:SorterLength) 
                     (switchFreq:SwitchFrequency)
                     (sorterCount:SorterCount) 
                     (rngSorters:RngGen)
                     (rngTypeIds:RngType) =

        let poolId = GuidUtils.guidFromObjs(
                      seq { (degree :> obj); 
                            (sorterLength :> obj); 
                            (switchFreq :> obj); 
                            (sorterCount :> obj);
                            (rngSorters :> obj);} )

        let randyForIds = Rando.fromGuid rngTypeIds poolId
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
        {
            id=poolId;
            degree=degree; 
            sorterPoolMembers = sorterPoolMembers; 
        }


type SorterPoolRunParams = 
  {
     id:Guid;
     startingSorterPool:SorterPool2;
     runLength:GenerationNumber;
     sorterPoolUpdateParams:SorterPoolUpdateParams;
     rngGens:Map<string, RngGen>
  }
    

module SorterPoolUpdateParams =

    let ss (poolId:Guid) 
           (fitnessFunc:FitnessFunc2)
           (breederFrac:PoolFraction)
           (mutationType:SorterMutationType) 
           (poolCount:SorterCount)
           (winnerFrac:PoolFraction) =

        let breederCount = PoolFraction.boundedMultiply breederFrac (SorterCount.value poolCount)
        let winnerCount = PoolFraction.boundedMultiply winnerFrac (SorterCount.value poolCount)

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
            poolCount = poolCount;
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
                           (runParamsCount:int) =

        let makeSorterPoolRunParams dex rngGenMut = 
            SorterPoolRunParams.ss (startingSorterPoolGen dex)
                                   rngGenMut
                                   (runLengthGen dex)
                                   (sorterPoolUpdateParamsGen dex)

        let sorterPoolRunParamsSet = RandoCollections.IndexedSeedGen rngGenMut
                                      |> Seq.map(fun (dex, rng) -> makeSorterPoolRunParams dex rng)
                                      |> Seq.take runParamsCount
                                      |> Seq.toArray
        {
            id=id;
            sorterPoolRunParamsSet = sorterPoolRunParamsSet
        }

    let yabba (degree:Degree)
              (sorterLength:SorterLength)
              (poolSize:SorterCount)
              (reps:ReplicaCount)
              (rngSorters:RngGen)
              (rngGenMut:RngGen)
              (poolCount:PoolCount)
              (runLength:GenerationNumber) =

              let startingSorterPoolGen (dex:int) =
                  SorterPool2.createRandom degree sorterLength

              None
              

//let createRandom (degree:Degree) 
//                 (sorterLength:SorterLength) 
//                 (switchFreq:float)
//                 (sorterCount:SorterCount) 
//                 (rngSorters:RngGen)
//                 (rngTypeIds:RngType) =