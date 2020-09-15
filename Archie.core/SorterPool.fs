namespace Archie.Base
open System


type PoolMemberState =
    | Legacy
    | Tested
    | Initiate
    | Archived
    | Root


type SorterPoolMember = {
        id:Guid;
        poolMemberState:PoolMemberState; 
        birthDate:GenerationNumber; 
        parent:SorterPoolMember option;
        sorter:Sorter;
        poolMemberRank:PoolMemberRank option;
        testResults:StandardSorterTestResults option;
        fitness:SorterFitness option;
    }

module SorterPoolMember =
    let makeRoot rootSorter res =
        {
            id=Guid.NewGuid();
            poolMemberState=PoolMemberState.Root; 
            birthDate=GenerationNumber.fromInt 0; 
            parent=None;
            sorter=rootSorter;
            poolMemberRank=None;
            testResults=res;
            fitness=None;
        }

    let archive spm =
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

    let getAdjFitness (pm:SorterPoolMember) (fitness:SorterFitness) =
        match pm.poolMemberState with
        | Legacy -> SorterFitness.fromFloat(SorterFitness.value(pm.fitness |> Option.get) + SorterFitness.value(fitness))
        | Tested -> pm.fitness |> Option.get
        | Initiate _ -> failwith "cannot getFitness from Initiate"
        | Archived _ -> failwith "cannot getFitness from Archived"
        | Root _ -> failwith "cannot getFitness from None"

    let getFitness (pm:SorterPoolMember) =
        match pm.poolMemberState with
        | Legacy | Tested -> pm.fitness |> Option.get
        | Initiate _ -> failwith "cannot getFitness from Initiate"
        | Archived _ -> failwith "cannot getFitness from Archived"
        | Root _ -> failwith "cannot getFitness from None"

    let toLegacy (pm:SorterPoolMember) (rank:PoolMemberRank) =
        match pm.poolMemberState with
        | Legacy -> {   SorterPoolMember.id = pm.id;
                        poolMemberState=PoolMemberState.Legacy;
                        birthDate=pm.birthDate;
                        parent=pm.parent;
                        sorter=pm.sorter;
                        poolMemberRank=Some rank;
                        testResults=pm.testResults;
                        fitness=pm.fitness;}
        | Tested -> {   SorterPoolMember.id = pm.id;
                        poolMemberState=PoolMemberState.Legacy;
                        birthDate=pm.birthDate;
                        parent=pm.parent;
                        sorter=pm.sorter;
                        poolMemberRank=Some rank;
                        testResults=pm.testResults;
                        fitness=pm.fitness;}
        | Initiate -> failwith "cannot convert Initiate to Legacy"
        | Archived _ -> failwith "cannot convert Archived to Legacy"
        | Root _ -> failwith "cannot convert None to Legacy"

    let mutate (mutator:Sorter->Sorter) (pm:SorterPoolMember) (generation:GenerationNumber) =
        match pm.poolMemberState with
        | Legacy -> {   SorterPoolMember.id = pm.id;
                        poolMemberState=PoolMemberState.Initiate;
                        birthDate=generation;
                        parent=Some pm;
                        sorter= (mutator pm.sorter);
                        poolMemberRank=None;
                        testResults=None;
                        fitness=None;}
        | Tested -> {   SorterPoolMember.id = pm.id;
                        poolMemberState=PoolMemberState.Initiate;
                        birthDate=generation;
                        parent=Some pm;
                        sorter= mutator pm.sorter;
                        poolMemberRank=None;
                        testResults=None;
                        fitness=None;}
        | Initiate -> { SorterPoolMember.id = pm.id;
                        poolMemberState=PoolMemberState.Initiate;
                        birthDate=generation;
                        parent=Some pm;
                        sorter= (mutator pm.sorter);
                        poolMemberRank=None;
                        testResults=None;
                        fitness=None;}
        | Archived _ -> failwith "cannot mutate Archived"
        | Root _ -> failwith "cannot mutate None"


    let parentReportHeader = [|"parentId"; "parentBirthdate"; "parentRank"; "parentFitness";|]


    let reportParent (spm:SorterPoolMember) =
        match spm.parent with
        | Some p ->
                match p.poolMemberState with
                | Legacy |Tested |Initiate -> [|(sprintf "%A" p.id);
                                                (sprintf "%d" (GenerationNumber.value (p.birthDate))); 
                                                (sprintf "%s" (PoolMemberRank.repStr p.poolMemberRank));
                                                (sprintf ".2%f" (SorterFitness.value (getFitness p)))|]
                | _ -> [|"";"";"";""|]
        | None -> [|"";"";"";""|]


    let reportHeader =  StandardSorterTestResults.headers
                        |> Array.append parentReportHeader
                        |> Array.append [|"sorter_id"; "birthdate"; "rank"; "fitness"|]


    let report (spm:SorterPoolMember) =
          (StandardSorterTestResults.reportOpt (spm.testResults))
          |> Array.append (reportParent (spm))
          |> Array.append [|(sprintf "%A" spm.id); 
                            (sprintf "%d" (GenerationNumber.value (spm.birthDate))); 
                            (sprintf "%s" (PoolMemberRank.repStr spm.poolMemberRank)); 
                            (sprintf "%s" (SorterFitness.repStr spm.fitness));|]



module SorterRun =

    let RunHeadersForBnW =
        let ha = SorterPoolMember.reportHeader
                |> Array.append PoolUpdateParamsBnW.headers
                |> Array.append [|"RunId"|]
        StringUtils.printArrayAsTabDelimited ha

    let RunReportForBnW (runid:Guid) (prams:PoolUpdateParamsBnW) (spm:SorterPoolMember) =
        let litem =
            SorterPoolMember.report spm
            |> Array.append (PoolUpdateParamsBnW.report prams)
            |> Array.append [|(sprintf "%A" runid)|]
        StringUtils.printArrayAsTabDelimited litem

    let NextGen (prams:PoolUpdateParamsBnW) (spms:SorterPoolMember[]) 
                (gen:GenerationNumber) =
        let mutator = PoolUpdateParamsBnW.mutator prams
        let breederCount = PoolFraction.boundedMultiply prams.breederFrac spms.Length
        let winnerCount = PoolFraction.boundedMultiply prams.winnerFrac spms.Length
        let mutantCount = SorterCount.value prams.poolCount - winnerCount

        let fitnessRankedMembers =
            spms
            |> Array.map(fun w ->
                (w, SorterFitness.value (SorterPoolMember.getAdjFitness w prams.legacyBias)))
            |> Array.sortByDescending(snd)

        let stsBreeders = fitnessRankedMembers |> Array.take(breederCount)

        let stsWinners = fitnessRankedMembers
                        |> Seq.take(winnerCount)
                        |> Seq.mapi(fun rankM tup-> SorterPoolMember.toLegacy (fst tup) (PoolMemberRank.fromInt (rankM + 1)))
                        |> Seq.toArray
        let stsMutants = stsBreeders 
                        |> CollectionUtils.IterateCircular mutantCount
                        |> Seq.map(fun tup-> SorterPoolMember.mutate mutator (fst tup) gen)
                        |> Seq.toArray
        stsMutants |> Array.append stsWinners


    let RunBnW (sorterPoolMember:SorterPoolMember) 
               (poolUpdateParamsBnW:PoolUpdateParamsBnW) 
               (sortableSet:SortableSet) 
               logToFile =

        let runId = Guid.NewGuid()
        logToFile (RunReportForBnW runId poolUpdateParamsBnW sorterPoolMember) true
        true


    let RunPoolOfBnW (logfile:string)
                     (degree:Degree)
                     (initialSwitchFrequency:float)
                     (initialConditionCount:InitialConditionCount)
                     (mutationRate:MutationRate)
                     (rngGenParams:RngGen) 
                     (replicaCount:ReplicaCount)
                     (rngGenSorters:RngGen)
                     (wRtGen:SwitchOrStage)
                     (wRtMut:SwitchOrStage)
                     (poolGenCount:PoolGenCount)
                     (useParallelProc:UseParallel)
                     (useEagerProc:UseEagerProc) =

        let LogToFile = LogUtils.logFile logfile
        LogToFile (sprintf "starting RunPoolOfBnW at %s" (System.DateTime.Now.ToString "hh:mm:ss") ) false
   
        let sorterLength = SorterLength.to999Sucessful degree wRtGen |> Result.ExtractOrThrow
        let sorterCount = SorterCount.fromInt (InitialConditionCount.value initialConditionCount)

        let paramReport =
            sprintf "degree:%d 
                     initialSwitchFrequency:%f
                     initialConditionCount:%d 
                     mutationRate:%f 
                     rngGenParams:%s 
                     rngGenSorters:%s
                     replicaCount:%d 
                     sorterLength:%s 
                     wRtGen:%A 
                     wRtMut:%A
                     poolGenCount:%d
                     parallelPr:%b
                     eagerPr: %b" 
                     (Degree.value degree)
                     initialSwitchFrequency
                     (InitialConditionCount.value initialConditionCount)
                     (MutationRate.value mutationRate)
                     (RngGenDto.toJson rngGenParams)
                     (RngGenDto.toJson rngGenSorters)
                     (ReplicaCount.value replicaCount)
                     (SorterLengthDto.toJson sorterLength)
                     wRtGen
                     wRtMut
                     (PoolGenCount.value poolGenCount)
                     (UseParallel.value useParallelProc)
                     (UseEagerProc.value useEagerProc)

        LogToFile paramReport true
        LogToFile RunHeadersForBnW true

        let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow
        let sorterRando = Rando.fromRngGen rngGenSorters
        let sorters = Sorter.createRandomArray degree sorterLength (Some initialSwitchFrequency) sorterRando sorterCount
        
        let sorterPoolMembers = SorterOps.GetStandardSortingResultsEager sortableSet (UseParallel.create false) sorters
                                |> Array.map(fun tup -> SorterPoolMember.makeRoot (fst tup) (Some (snd tup)))

        let sorterAndPrams = PoolUpdateParamsBnW.ParamsSnS rngGenParams poolGenCount
                                |> Seq.take (ReplicaCount.value replicaCount)
                                |> Seq.toArray
                                |> Array.allPairs sorterPoolMembers

        let _res = sorterAndPrams 
                   |> Array.map(fun q -> RunBnW (fst q) (snd q) sortableSet LogToFile)

        //let paramRndGen = RngGen.createLcg paramSeed
        //let sorterRando = Rando.LcgFromSeed sorterSeed

        "RunPoolOfBnW is done"