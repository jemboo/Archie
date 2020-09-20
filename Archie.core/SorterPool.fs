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

    let makeRoot rootSorter res fit =
        {
            id=Guid.NewGuid();
            poolMemberState=PoolMemberState.Root; 
            birthDate=GenerationNumber.fromInt 0; 
            parent=None;
            sorter=rootSorter;
            poolMemberRank=None;
            testResults=res;
            fitness=fit;
        }

    let toArchived spm =
        match spm.poolMemberState with
        | Legacy | Tested | Initiate ->
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

        
    let toInitiate (mutator:Sorter->Sorter) (pm:SorterPoolMember) (generation:GenerationNumber) =
            { SorterPoolMember.id = Guid.NewGuid();
            poolMemberState=PoolMemberState.Initiate;
            birthDate=generation;
            parent=Some pm;
            sorter= (mutator pm.sorter);
            poolMemberRank=None;
            testResults=None;
            fitness=None;}


    let toLegacy (pm:SorterPoolMember) (rank:PoolMemberRank) =
        match pm.poolMemberState with
        | Legacy -> {SorterPoolMember.id = pm.id;
                    poolMemberState=PoolMemberState.Legacy;
                    birthDate=pm.birthDate;
                    parent=pm.parent;
                    sorter=pm.sorter;
                    poolMemberRank=Some rank;
                    testResults=pm.testResults;
                    fitness=pm.fitness;}
        | Tested -> {SorterPoolMember.id = pm.id;
                    poolMemberState=PoolMemberState.Legacy;
                    birthDate=pm.birthDate;
                    parent=pm.parent;
                    sorter=pm.sorter;
                    poolMemberRank=Some rank;
                    testResults=pm.testResults;
                    fitness=pm.fitness;}
        | Initiate -> failwith "cannot convert Initiate to Legacy"
        | Archived _ -> failwith "cannot convert Archived to Legacy"
        | Root _ -> failwith "cannot convert Root to Legacy"


    let toTested (pm:SorterPoolMember) (eval:Sorter->StandardSorterTestResults) 
                 (fitnessFunc:FitnessFunc) (gen:GenerationNumber) =
        match pm.poolMemberState with
        | Legacy -> pm
        | Tested -> pm
        | Root -> 
            let r = eval pm.sorter
            let f = fitnessFunc.func r gen
            {SorterPoolMember.id = pm.id;
             poolMemberState = PoolMemberState.Tested;
             birthDate = pm.birthDate;
             parent = Some pm;
             sorter = pm.sorter;
             poolMemberRank = None;
             testResults = pm.testResults;
             fitness = Some f;}
        | Initiate -> 
            let r = eval pm.sorter
            let f = fitnessFunc.func r gen
            {SorterPoolMember.id = pm.id;
             poolMemberState = PoolMemberState.Tested;
             birthDate = pm.birthDate;
             parent = pm.parent;
             sorter = pm.sorter;
             poolMemberRank = None;
             testResults = Some r;
             fitness = Some f;}
        | Archived _ -> failwith "cannot convert Archived to Tested"



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


    let isTopRanked (pm:SorterPoolMember) =
       match pm.poolMemberState with
       | Legacy -> let r = pm.poolMemberRank |> Option.get
                   (PoolMemberRank.value r) = 1
       | Tested -> false
       | Initiate _ -> false
       | Archived _ -> false
       | Root _ -> false

    let parentReportHeader = [|"parentId"; "parentBirthdate"; "parentRank"; "parentFitness";|]


    let reportParent (spm:SorterPoolMember) =
        match spm.parent with
        | Some p ->
                match p.poolMemberState with
                | Legacy |Tested |Initiate -> [|(sprintf "%A" p.id);
                                                (sprintf "%d" (GenerationNumber.value (p.birthDate))); 
                                                (sprintf "%s" (PoolMemberRank.repStr p.poolMemberRank));
                                                (sprintf "%.4f" (SorterFitness.value (getFitness p)))|]
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


    let NextGen (prams:PoolUpdateParamsBnW) (sorterPool:SorterPoolMember[]) 
                (gen:GenerationNumber) (rando:IRando) =
        let mutator = PoolUpdateParamsBnW.mutator prams rando
        let breederCount = PoolFraction.boundedMultiply prams.breederFrac sorterPool.Length
        let winnerCount = PoolFraction.boundedMultiply prams.winnerFrac sorterPool.Length
        let mutantCount = SorterCount.value prams.poolCount - winnerCount

        let fitnessRankedMembers =
            sorterPool
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
                        |> Seq.map(fun tup-> SorterPoolMember.toInitiate mutator (fst tup) gen)
                        |> Seq.toArray
        stsMutants |> Array.append stsWinners


    let RunBnW (sorterPoolMember:SorterPoolMember) 
               (poolUpdateParamsBnW:PoolUpdateParamsBnW) 
               (sortableSet:SortableSet) 
               (logToFile:(int->Guid->string->unit)) =

        let testSPM (spm:SorterPoolMember) (gen:GenerationNumber) = 
            let eval = SorterOps.GetTheStandardSortingResultsComplete sortableSet
            SorterPoolMember.toTested spm eval (poolUpdateParamsBnW.fitnessFunc) gen

        let runId = Guid.NewGuid()
        let rando = (Rando.fromRngGen poolUpdateParamsBnW.rngGen)
        logToFile 0 runId (RunReportForBnW runId poolUpdateParamsBnW sorterPoolMember)

        let mutable sorterPool = [|sorterPoolMember|]
        let mutable gen = GenerationNumber.fromInt 1
        let mutable lastWinningBirtdate = GenerationNumber.fromInt 0
        while ((GenerationNumber.value gen) < (GenerationNumber.value poolUpdateParamsBnW.generationNumber)) do
            sorterPool <-sorterPool |> Array.map(fun spm-> testSPM spm gen)
            sorterPool <- NextGen poolUpdateParamsBnW sorterPool gen rando
            let winner = sorterPool |> Array.filter(fun pm -> SorterPoolMember.isTopRanked pm) |> Array.exactlyOne
            if winner.birthDate <> lastWinningBirtdate then
               Console.WriteLine(sprintf "%A %d" runId (GenerationNumber.value gen))
               logToFile  (GenerationNumber.value gen) runId (RunReportForBnW runId poolUpdateParamsBnW winner)
               lastWinningBirtdate <- winner.birthDate
            gen <- GenerationNumber.fromInt ((GenerationNumber.value gen) + 1)
        true


    let RunPoolOfBnW (logfile:string)
                     (initialConditionCount:InitialConditionCount)
                     (replicaCount:ReplicaCount)
                     (degree:Degree)
                     (rngGenSorters:RngGen)
                     (wOrTGen:SwitchOrStage)
                     (initialSwitchFrequency:float)
                     (poolGenCount:PoolGenCount)
                     (poolCount:SorterCount)
                     (rngGenParams:RngGen) 
                     (legacyBias:SorterFitness)
                     (sorterMutationType:SorterMutationType)
                     (useParallelProc:UseParallel)
                     (useEagerProc:UseEagerProc) =

        let LogToFile = LogUtils.logFile logfile
        LogToFile (sprintf "starting RunPoolOfBnW at %s" (System.DateTime.Now.ToString "hh:mm:ss") ) false
   
        let sorterLength = SorterLength.to999Sucessful degree wOrTGen |> Result.ExtractOrThrow
        let sorterCount = SorterCount.fromInt (InitialConditionCount.value initialConditionCount)

        let paramReport =
            sprintf "initialConditionCount:%d
                     replicaCount:%d 
                     degree:%d
                     rngGenSorters:%s
                     wOrTGen:%A 
                     initialSwitchFrequency:%f
                     poolGenCount:%d
                     rngGenParams:%s 
                     legacyBias:%f
                     sorterMutationType:%s
                     parallelPr:%b
                     eagerPr:%b
                     sorterLength:%s"
                     (InitialConditionCount.value initialConditionCount)
                     (ReplicaCount.value replicaCount)
                     (Degree.value degree)
                     (RngGenDto.toJson rngGenSorters)
                     wOrTGen
                     initialSwitchFrequency
                     (PoolGenCount.value poolGenCount)
                     (RngGenDto.toJson rngGenParams)
                     (SorterFitness.value legacyBias)
                     (sorterMutationType |> SorterMutationTypeDto.toJson)
                     (UseParallel.value useParallelProc)
                     (UseEagerProc.value useEagerProc)
                     (SorterLengthDto.toJson sorterLength)

        LogToFile paramReport true
        let cumer = LogUtils.logFileKeyHeader logfile RunHeadersForBnW
        let logToFileKey = LogUtils.logFileKey logfile cumer

        let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow
        let sorterRando = Rando.fromRngGen rngGenSorters
        let sorters = Sorter.createRandomArray degree sorterLength 
                             (Some initialSwitchFrequency) sorterRando sorterCount
        
        let sorterPoolMembers = SorterOps.GetStandardSortingResultsEager sortableSet (UseParallel.create false) sorters
                                |> Array.map(fun tup -> SorterPoolMember.makeRoot (fst tup) (Some (snd tup)) None)

        let sorterAndPrams = PoolUpdateParamsBnW.ParamsSnS rngGenParams poolGenCount poolCount
                                sorterMutationType legacyBias 1024.0 0.0 0.0005 0.0005
                                |> Seq.take (ReplicaCount.value replicaCount) 
                                |> Seq.toArray
                                |> Array.allPairs sorterPoolMembers

        let _res = sorterAndPrams 
                   |> Array.map(fun q -> RunBnW (fst q) (snd q) sortableSet logToFileKey |> ignore
                                         LogUtils.logFileBackfill logfile cumer)

        //let paramRndGen = RngGen.createLcg paramSeed
        //let sorterRando = Rando.LcgFromSeed sorterSeed

        "RunPoolOfBnW is done"