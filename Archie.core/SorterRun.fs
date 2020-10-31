namespace Archie.Base
open System


module SorterRun =

    let RunHeadersForBnW =
        let ha = SorterPoolMember2.reportHeader
                |> Array.append PoolUpdateParamsBnW.headers
                |> Array.append [|"RunId"|]
        StringUtils.printArrayAsTabDelimited ha


    let RunReportForBnW (runid:Guid) (prams:PoolUpdateParamsBnW) (spm:SorterPoolMember2) =
        let litem =
            SorterPoolMember2.report spm
            |> Array.append (PoolUpdateParamsBnW.report prams)
            |> Array.append [|(sprintf "%A" runid)|]
        StringUtils.printArrayAsTabDelimited litem


    let Measure (sorterPool:SorterPoolMember2[]) (sortableSet:SortableSet)  =
        sorterPool
        |> Array.map(fun spm-> let eval = SorterOps.GetTheStandardSortingResultsComplete sortableSet
                               SorterPoolMember2.toMeasured spm eval)
        |> Array.toList
        |> Result.sequence
        |> Result.ExtractOrThrow
        |> List.toArray


    let Evaluate (prams:PoolUpdateParamsBnW) (sorterPool:SorterPoolMember2[]) 
                 (fitnessFuncParam:FitnessFuncParam) =
        sorterPool
        |> Array.map(fun pm-> match pm.poolMemberState with
                              | Legacy -> pm
                              | _ -> (SorterPoolMember2.toEvaluated pm prams.fitnessFunc
                                                       fitnessFuncParam) |> Result.ExtractOrThrow)


    let NextGen (prams:PoolUpdateParamsBnW) (sorterPool:SorterPoolMember2[]) 
                (rando:IRando) (gen:GenerationNumber) =
        let archiver = fun (spm:SorterPoolMember2) -> spm |> ignore
        let mutator = PoolUpdateParamsBnW.mutator prams rando
        let breederCount = PoolFraction.boundedMultiply prams.breederFrac sorterPool.Length
        let winnerCount = PoolFraction.boundedMultiply prams.winnerFrac sorterPool.Length
        let mutantCount = SorterCount.value prams.poolCount - winnerCount

        let fitnessRankedMembers =
            sorterPool
            |> Array.map(fun w ->
                (w, (SorterFitness.value (w.fitness |> Option.get))))
            |> Array.sortByDescending(snd)

        let stsBreeders = fitnessRankedMembers |> Array.take(breederCount)

        let stsWinners = fitnessRankedMembers
                        |> Seq.take(winnerCount)
                        |> Seq.mapi(fun rankM tup -> SorterPoolMember2.toLegacy 
                                                      (fst tup) 
                                                      (PoolMemberRank.fromInt (rankM + 1)))
                        |> Seq.toList
                        |> Result.sequence
                        |> Result.ExtractOrThrow
                        |> List.toArray
        let stsMutants = stsBreeders 
                        |> CollectionUtils.IterateCircular mutantCount
                        |> Seq.map(fun tup-> SorterPoolMember2.toInitiate mutator (fst tup) gen)
                        |> Seq.toArray
        stsMutants |> Array.append stsWinners


    let RunBnW (sorterPoolMember:SorterPoolMember2) 
               (poolUpdateParamsBnW:PoolUpdateParamsBnW) 
               (sortableSet:SortableSet) 
               (logToFile:(int->Guid->string->unit)) =

        let runId = poolUpdateParamsBnW.id |> GuidUtils.addGuids sorterPoolMember.id
        logToFile 0 runId (RunReportForBnW runId poolUpdateParamsBnW sorterPoolMember)
        let rando = Rando.fromGuid poolUpdateParamsBnW.rngType poolUpdateParamsBnW.id

        let mutable sorterPool = [|sorterPoolMember|]
        let mutable gen = GenerationNumber.fromInt 1
        let mutable lastWinningBirtdate = GenerationNumber.fromInt 0
        while ((GenerationNumber.value gen) < (GenerationNumber.value poolUpdateParamsBnW.runLength)) do
            sorterPool <- Measure sorterPool sortableSet
            sorterPool <- Evaluate poolUpdateParamsBnW sorterPool (FitnessFuncParam.NoParam)
            sorterPool <- NextGen poolUpdateParamsBnW sorterPool rando gen
            let winner = sorterPool |> Array.filter(fun pm -> SorterPoolMember2.isTopRanked pm) |> Array.exactlyOne
            if winner.birthDate <> lastWinningBirtdate then
               Console.WriteLine(sprintf "%A %d" runId (GenerationNumber.value gen))
               logToFile  (GenerationNumber.value gen) runId (RunReportForBnW runId poolUpdateParamsBnW winner)
               lastWinningBirtdate <- winner.birthDate
            gen <- GenerationNumber.increment gen
        true


    let RunPoolOfBnW (logfile:string)
                     (initialConditionCount:InitialConditionCount)
                     (replicaCount:ReplicaCount)
                     (degree:Degree)
                     (rngGenSorters:RngGen)
                     (wOrTGen:SwitchOrStage)
                     (initialSwitchFrequency:SwitchFrequency)
                     (poolGenCount:PoolGenCount)
                     (poolCount:SorterCount)
                     (rngGenParams:RngGen) 
                     (legacyBias:SorterFitness)
                     (sorterMutationType:SorterMutationType)
                     (useParallelProc:UseParallel)
                     (useEagerProc:UseEagerProc) =

        let LogToFile = LogUtils.logFile logfile
        LogToFile (sprintf "starting RunPoolOfBnW at %s" (System.DateTime.Now.ToString "hh:mm:ss") ) false
   
       // let sorterLength = SorterLength.to999Sucessful degree wOrTGen
       // let sorterLength = SorterLength.toRecordSorterLength degree //(SorterLength.makeStageCount 1)
        let sorterLength = SorterLength.toRecordSorterLengthPlus degree (SorterLength.makeStageCount 2)//  (SorterLength.Stage (StageCount.fromInt 2))
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
                     (SwitchFrequency.value initialSwitchFrequency)
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
                                |> Array.map(fun tup -> SorterPoolMember2.makeRoot (Guid.NewGuid()) (fst tup) (Some (snd tup)) None)

        //let sorterAndPrams = PoolUpdateParamsBnW.ParamsSnS rngGenParams poolGenCount poolCount
        //                        sorterMutationType legacyBias 1024.0 0.0 0.0005 0.0005
        //                        |> Seq.take (ReplicaCount.value replicaCount) 
        //                        |> Seq.toArray
        //                        |> Array.allPairs sorterPoolMembers

        let sorterAndPrams = PoolUpdateParamsBnW.ParamsMr rngGenParams poolGenCount
                                |> Seq.take (ReplicaCount.value replicaCount) 
                                |> Seq.toArray
                                |> Array.allPairs sorterPoolMembers


        let _res = sorterAndPrams 
                   |> Array.map(fun q -> RunBnW (fst q) (snd q) sortableSet logToFileKey |> ignore
                                         LogUtils.logFileBackfill logfile cumer)

        //let paramRndGen = RngGen.createLcg paramSeed
        //let sorterRando = Rando.LcgFromSeed sorterSeed

        "RunPoolOfBnW is done"





//// Params for Random walk process
//type RwUpdateParams = 
//    {generationNumber:GenerationNumber;
//    mutationType:SorterMutationType;
//    poolCount:SorterCount;
//    rngGen:RngGen;
//    fitnessFunc:FitnessFunc;}


//module RwUpdateParams =

//  let Params10 (rngGen:RngGen) (poolSize:int) =
//    let pm rg dex =
//        let gc, pc = ParamUtils.SplitPoolGenBnWFrac poolSize dex
//        {
//            generationNumber = gc;
//            mutationType=SorterMutationType.Switch (MutationRate.fromFloat 0.02);
//            poolCount=SorterCount.fromInt pc;
//            rngGen=rg;
//            fitnessFunc=FitnessFunc.standardSwitch 4.0;
//        }
//    RandoCollections.IndexedSeedGen rngGen |> Seq.map(fun (dex, rg) -> pm rg dex)

//  let ParamsM (rngGen:RngGen) (poolCount:int) =
//      let pm rg dex =
//          let gc, pc, mut = ParamUtils.SplitPoolGenMut2 poolCount dex
//          {
//              generationNumber = gc;
//              mutationType = mut;
//              poolCount = pc;
//              rngGen = rg;
//              fitnessFunc=FitnessFunc.standardSwitch 4.0 ;
//          }
//      RandoCollections.IndexedSeedGen rngGen |> Seq.map(fun (dex, rg) -> pm rg dex)



//type RndSorterParams = 
//    {degree:Degree;
//     sorterCount:SorterCount;
//     rngGen:RngGen;
//     sorterLength:SorterLength}


//module RndSorterParams =

//    let Make (degree:Degree) (sorterCount:SorterCount) 
//             (rngGen:RngGen) (wOrT:SwitchOrStage) =
    
//        result {
//            let sorterLength = SorterLength.to999Sucessful degree wOrT
//            return 
//                {RndSorterParams.degree=degree;
//                sorterCount=sorterCount;
//                rngGen=rngGen;
//                sorterLength = sorterLength}
//        }


