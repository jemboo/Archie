namespace Archie.Base
open System


type PoolMemberState =
    | Root
    | Initiate
    | Measured
    | Evaluated
    | Legacy
    | Archived

type SorterPoolMember = {
    id:Guid;
    poolMemberState:PoolMemberState; 
    birthDate:GenerationNumber; 
    parent:SorterPoolMemberRef option;
    sorter:Sorter;
    poolMemberRank:PoolMemberRank option;
    testResults:SorterTestResults option;
    fitness:SorterFitness option;
}
and SorterPoolMemberRef = R of SorterPoolMember | I of Guid


type FitnessFuncCat =
     | StandardSwitch
     | StandardStage
     | AltSwitchAndStage of GenerationNumber
     | AltSwitchStageAndSuccess of GenerationNumber


module FitnessFuncCat =
    let report (ffc:FitnessFuncCat) =
        match ffc with
        | StandardSwitch ->  ("StandardSwitch", "")
        | StandardStage ->  ("StandardStage", "")
        | AltSwitchAndStage g ->  ("AltSwitchAndStage", sprintf "%d" (GenerationNumber.value g))
        | AltSwitchStageAndSuccess g -> ("AltSwitchStageAndSuccess", sprintf "%d" (GenerationNumber.value g))


type FitnessFuncParam =
     | NoParam
     | Gen of GenerationNumber
     | Pool of GenerationNumber*SorterPoolMember[]

type FF = private FF of (FitnessFuncParam->SorterTestResults->SorterFitness) 


module FF =
    
    let create ff =
        (FF ff)

    let value (FF ff) = ff

    let standardSwitch = 
        let ff p r =
            match p with
            | NoParam->
                SorterFitness.fromFloat (float (SwitchCount.value r.usedSwitchCount) * -1.0)
            | _ -> failwith (sprintf "param: %A incorrect for standardSwitch" p)

        create ff

    let standardStage = 
        let ff p r =
            match p with
            | NoParam->
                SorterFitness.fromFloat (float (StageCount.value r.usedStageCount) * -1.0)
            | _ -> failwith (sprintf "param: %A incorrect for standardStage" p)

        create ff

    let altSwitchAndStage (cycleG:GenerationNumber) = 
        let ff p r =
            match p with
            | Gen g ->
                let phase = (GenerationNumber.value g) % ((GenerationNumber.value cycleG) * 2)
                if (phase < (GenerationNumber.value cycleG)) then
                   SorterFitness.fromFloat (float (StageCount.value r.usedStageCount) * -1.0)
                else
                   SorterFitness.fromFloat (float (SwitchCount.value r.usedSwitchCount) * -1.0)

            | _ -> failwith (sprintf "param: %A incorrect for altSwitchAndStage" p)

        create ff


    let altSwitchStageAndSuccess (cycleG:GenerationNumber) = 
        let ff p r =
            match p with
            | Gen g ->
                let phase = (GenerationNumber.value g) % ((GenerationNumber.value cycleG) * 3)
                if (phase < (GenerationNumber.value cycleG)) then
                   SorterFitness.fromFloat (float (StageCount.value r.usedStageCount) * -1.0)
                elif (phase < (GenerationNumber.value cycleG) * 2) then
                   SorterFitness.fromFloat (float (SwitchCount.value r.usedSwitchCount) * -1.0)
                else
                   SorterFitness.fromFloat (float (SortableCount.value r.successfulSortCount))

            | _ -> failwith (sprintf "param: %A incorrect for altSwitchStageAndSuccess" p)

        create ff


type FitnessFunc = {cat:FitnessFuncCat; func:FF}

module PoolMemberState =

    let toDto (pms:PoolMemberState) =
        match pms with
              | Root -> "Root"
              | Initiate -> "Initiate"
              | Measured -> "Measured"
              | Evaluated -> "Evaluated"
              | Legacy -> "Legacy"
              | Archived -> "Archived"


    let fromDto (pms:string) =
        match pms with
              | "Root" -> Root |> Ok
              | "Initiate" -> Initiate |> Ok
              | "Measured" -> Measured |> Ok
              | "Evaluated" -> Evaluated |> Ok
              | "Legacy" -> Legacy |> Ok
              | "Archived" -> Archived |> Ok
              | _ -> Error (sprintf "%s not handled" pms)


module SorterPoolMember =

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


    let getFitness (pm:SorterPoolMember) =
       match pm.poolMemberState with
       | Legacy | Evaluated -> 
            match (pm.fitness) with
            | Some f -> f |> Result.Ok
            | None -> "fitness value missing" |> Result.Error
       | Measured -> "cannot getFitness from Measured" |> Result.Error
       | Initiate _ -> "cannot getFitness from Initiate" |> Result.Error
       | Archived _ -> "cannot getFitness from Archived" |> Result.Error
       | Root _ -> "cannot getFitness from None" |> Result.Error


    let getFitnessReport (pm:SorterPoolMember) (errNum:float) =
        match (getFitness pm) with
        | Error r -> errNum
        | Ok f -> (SorterFitness.value f)


    let isTopRanked (pm:SorterPoolMember) =
       match pm.poolMemberRank with
       | Some r -> r = (PoolMemberRank.fromInt 1)
       | None -> false


    let getParentId (spm:SorterPoolMember) =
        match spm.parent with
         | Some r -> 
            match r with
                    | I guey -> Some guey
                    | R rfey -> Some rfey.id
         | None -> None


    let attachParent (spmParent:SorterPoolMember) (spm:SorterPoolMember) =
        match (getParentId spm) with
        | None -> "Parent Id missing" |> Result.Error
        | Some id -> if (id = spmParent.id) then
                        {
                            SorterPoolMember.id=spm.id;
                            poolMemberState=spm.poolMemberState; 
                            birthDate=spm.birthDate; 
                            parent= Some (SorterPoolMemberRef.R spmParent);
                            sorter=spm.sorter;
                            poolMemberRank=spm.poolMemberRank;
                            testResults=spm.testResults;
                            fitness=spm.fitness;
                        } |> Ok
                     else
                        "Parent Id's do not match" |> Result.Error


    let toArchived (spm:SorterPoolMember) =
        match (getParentId spm) with
        | None -> 
            {
                SorterPoolMember.id=spm.id;
                poolMemberState=PoolMemberState.Archived; 
                birthDate=spm.birthDate; 
                parent=None;
                sorter=spm.sorter;
                poolMemberRank=spm.poolMemberRank;
                testResults=spm.testResults;
                fitness=spm.fitness;
            } |> Ok
        | Some guey ->
            {
                SorterPoolMember.id=spm.id;
                poolMemberState=PoolMemberState.Archived; 
                birthDate=spm.birthDate; 
                parent= Some (SorterPoolMemberRef.I guey);
                sorter=spm.sorter;
                poolMemberRank=spm.poolMemberRank;
                testResults=spm.testResults;
                fitness=spm.fitness;
            } |> Ok


    let toInitiate (mutator:Sorter->Sorter) (parent:SorterPoolMember) 
           (generation:GenerationNumber) =
        { 
            SorterPoolMember.id = Guid.NewGuid();
            poolMemberState=PoolMemberState.Initiate;
            birthDate=generation;
            parent= Some (SorterPoolMemberRef.R parent);
            sorter= (mutator parent.sorter);
            poolMemberRank=None;
            testResults=None;
            fitness=None;
        }

    let toMeasured (pm:SorterPoolMember) (measure:Sorter->SorterTestResults) =
        match pm.poolMemberState with
        | Archived _ -> "cannot convert Archived to Measured" |> Result.Error

        | _ ->
            {SorterPoolMember.id = pm.id;
             poolMemberState = PoolMemberState.Measured;
             birthDate = pm.birthDate;
             parent = pm.parent;
             sorter = pm.sorter;
             poolMemberRank = None;
             testResults = Some (measure pm.sorter);
             fitness = None;} |> Result.Ok

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
             |> Result.Ok
        | Root -> "cannot convert Root to Evaluated" |> Result.Error
        | Initiate -> "cannot convert Initiate to Evaluated" |> Result.Error
        | Archived -> "cannot convert Archived to Evaluated" |> Result.Error


    let toLegacy (pm:SorterPoolMember) 
                 (rank:PoolMemberRank) =

        match pm.poolMemberState with
        | Evaluated -> 
            match (getParentId pm) with
                 | None ->
                     {
                         SorterPoolMember.id = pm.id;
                          poolMemberState=PoolMemberState.Legacy;
                          birthDate=pm.birthDate;
                          parent = None;
                          sorter=pm.sorter;
                          poolMemberRank=Some rank;
                          testResults=pm.testResults;
                          fitness=pm.fitness;
                      } |> Result.Ok
                 | Some guey ->
                    {
                        SorterPoolMember.id = pm.id;
                         poolMemberState=PoolMemberState.Legacy;
                         birthDate=pm.birthDate;
                         parent = Some (SorterPoolMemberRef.I guey);
                         sorter=pm.sorter;
                         poolMemberRank=Some rank;
                         testResults=pm.testResults;
                         fitness=pm.fitness;
                     } |> Result.Ok
        | Legacy -> "cannot convert Legacy to Legacy" |> Result.Error
        | Measured -> "cannot convert Measured to Legacy" |> Result.Error
        | Initiate -> "cannot convert Initiate to Legacy" |> Result.Error
        | Archived -> "cannot convert Archived to Legacy" |> Result.Error
        | Root -> "cannot convert Root to Legacy" |> Result.Error


    let parentReportHeader = [|"parentId"; "parentBirthdate"; "parentRank"; "parentFitness";|]

    let reportParent (spm:SorterPoolMember) =
        let yab = match spm.parent with
                  | Some r -> 
                    match r with
                          | I guey -> [|"";"";"";""|]
                          | R rfey -> 
                              match rfey.poolMemberState with
                              | Legacy | Evaluated | Initiate -> 
                                  [|(sprintf "%A" rfey.id);
                                  (sprintf "%d" (GenerationNumber.value (rfey.birthDate))); 
                                  (sprintf "%s" (PoolMemberRank.repStr rfey.poolMemberRank));
                                  (sprintf "%.4f" (getFitnessReport rfey -999.99))|]
                              | _ -> [|"";"";"";""|]
                  | None -> [|"";"";"";""|]
        yab


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




module FitnessFunc = 

    let standardSwitch = 
        {FitnessFunc.cat=FitnessFuncCat.StandardSwitch; func=FF.standardSwitch}

    let standardStage = 
        {FitnessFunc.cat=FitnessFuncCat.StandardStage; func=FF.standardStage}

    let altSwitchAndStage (cycleG:GenerationNumber) = 
        {FitnessFunc.cat=FitnessFuncCat.AltSwitchAndStage cycleG; func=FF.altSwitchAndStage cycleG}

    let altSwitchStageAndSuccess (cycleG:GenerationNumber) = 
        {FitnessFunc.cat=FitnessFuncCat.AltSwitchStageAndSuccess cycleG; func=FF.altSwitchStageAndSuccess cycleG}



type FitnessFuncDto = {cat:string; prams:Map<string, obj>}
module FitnessFuncDto =
    
    let standardSwitchDto = 
        {cat="StandardSwitch"; prams = Seq.empty |> Map.ofSeq}

    let standardStageDto = 
        {cat="StandardStage";  prams = Seq.empty |> Map.ofSeq}

    let altSwitchAndStageDto (cycleG:GenerationNumber) =  
        {cat="AltSwitchAndStage"; 
         prams = seq { ("cycleG", (GenerationNumber.value cycleG) :> obj) } |> Map.ofSeq}
       
    let altSwitchStageAndSuccessDto (cycleG:GenerationNumber) = 
        {cat="AltSwitchStageAndSuccess"; 
         prams = seq { ("cycleG", (GenerationNumber.value cycleG) :> obj) } |> Map.ofSeq}

    let fromDto (fitnessFuncDto:FitnessFuncDto) =
        match fitnessFuncDto.cat with
        | "StandardSwitch" -> FitnessFunc.standardSwitch |> Ok
        | "StandardStage" -> FitnessFunc.standardStage |> Ok
        | "AltSwitchAndStage" ->  
           result {
                    let! g = GenerationNumber.fromKey fitnessFuncDto.prams "cycleG"
                    return FitnessFunc.altSwitchAndStage g
                  }
        | "AltSwitchStageAndSuccess" ->
           result {
                     let! g = GenerationNumber.fromKey fitnessFuncDto.prams "cycleG"
                     return FitnessFunc.altSwitchStageAndSuccess g
                  }
        | _ -> sprintf "%s not handled" fitnessFuncDto.cat |> Error

    let toDto (ff:FitnessFunc) =
        match ff.cat with
              | StandardSwitch -> {cat ="StandardSwitch"; prams = Map.empty}
              | StandardStage -> {cat="StandardStage"; prams = Map.empty}
              | AltSwitchAndStage gen -> 
                {cat="AltSwitchAndStage"; 
                prams= seq { ("cycleG", (GenerationNumber.value gen) :> obj) } |> Map.ofSeq}
              | AltSwitchStageAndSuccess gen -> 
                {cat="AltSwitchStageAndSuccess"; 
                prams = seq { ("cycleG", (GenerationNumber.value gen) :> obj) } |> Map.ofSeq}


//type FitnessFuncCat =
//    | StandardSwitch
//    | StandardStage
//    | AltSwitchAndStage of GenerationNumber
//    | AltSwitchStageAndSuccess of GenerationNumber


type SM = private SM of (IRando->Sorter->Sorter)
module SM =
    let create ff = (SM ff)
    let value (SM ff) = ff
    let fromSorterMutationType smt =
        create (Sorter.mutate smt)


type SorterMutationCat =
    | SwitchBased of MutationRate
    | StageBased of MutationRate
    
type SorterMutation = {cat:SorterMutationCat; func:SM}
module SorterMutation = 

    let standardSwitch mr = 
        {SorterMutation.cat=SorterMutationCat.SwitchBased mr; 
        func=SM.fromSorterMutationType (SorterMutationType.Switch mr)}

    let standardStage mr = 
        {SorterMutation.cat=SorterMutationCat.StageBased mr; 
         func=SM.fromSorterMutationType (SorterMutationType.Stage mr)}

    let standardMutator (mutationType:SorterMutationType) =
        match mutationType with
        | SorterMutationType.Switch mr -> standardSwitch mr
        | SorterMutationType.Stage mr -> standardStage mr


type SorterMutationDto = {cat:string; prams:Map<string, obj>}
module SorterMutationDto =

    let SwitchBased (mutationRate:MutationRate) =  
        {cat="SwitchBased"; 
         prams = seq { ("mutationRate", (MutationRate.value mutationRate) :> obj) } |> Map.ofSeq}
       
    let StageBased (mutationRate:MutationRate) = 
        {cat="StageBased"; 
         prams = seq { ("mutationRate", (MutationRate.value mutationRate) :> obj) } |> Map.ofSeq}

    let fromDto (sorterMutationDto:SorterMutationDto) =
        match sorterMutationDto.cat with
        | "SwitchBased" ->  
           result {
                    let! mr = MutationRate.fromKey sorterMutationDto.prams "mutationRate"
                    return SorterMutation.standardSwitch mr
                  }
        | "StageBased" ->
           result {
                     let! mr = MutationRate.fromKey sorterMutationDto.prams "mutationRate"
                     return SorterMutation.standardStage mr
                  }
        | _ -> sprintf "%s not handled" sorterMutationDto.cat |> Error

    let toDto (sm:SorterMutation) =
        match sm.cat with
              | SwitchBased mr -> 
                    {cat="SwitchBased"; 
                     prams= seq { ("mutationRate", (MutationRate.value mr) :> obj) } |> Map.ofSeq}
              | StageBased mr -> 
                    {cat="StageBased"; 
                     prams = seq { ("mutationRate", (MutationRate.value mr) :> obj) } |> Map.ofSeq}



type PS = private PS of (SorterPoolMember[] -> SorterPoolMember[])
module PS =
    let create ff = (PS ff)
    let value (PS ff) = ff

    
type PoolSelectorCat =
        | Standard of PoolFraction

type PoolSelector2 = {cat:PoolSelectorCat; func:PS}
module PoolSelector2 =
    let standard (poolFraction:PoolFraction) =
        let ss = fun (sorterPoolMembers:SorterPoolMember[]) ->
                   let fractionCount = PoolFraction.boundedMultiply poolFraction sorterPoolMembers.Length
                   sorterPoolMembers
                   |> Array.map(fun w ->
                       (w, (SorterFitness.value ((SorterPoolMember.getFitness w) |> Result.ExtractOrThrow ))))
                   |> Array.sortByDescending(snd)
                   |> Array.map(fst)
                   |> Array.take(fractionCount)

        { cat = PoolSelectorCat.Standard poolFraction; func = PS.create ss }

        

type PoolSelector2Dto = {cat:string; prams:Map<string, obj>}
module PoolSelector2Dto =

    let standard (poolFraction:PoolFraction) =  
        {cat="standard"; 
         prams = seq { ("mutationRate", (PoolFraction.value poolFraction) :> obj) } |> Map.ofSeq}
       
    let fromDto (poolSelector2Dto:PoolSelector2Dto) =
        match poolSelector2Dto.cat with
        | "standard" ->  
           result {
                    let! mr = PoolFraction.fromKey poolSelector2Dto.prams "poolFraction"
                    return PoolSelector2.standard mr
                  }
        | _ -> sprintf "%s not handled" poolSelector2Dto.cat |> Error

    let toDto (ps:PoolSelector2) =
        match ps.cat with
              | Standard pf -> {cat="SwitchBased"; 
                                prams= seq { ("poolFraction", (PoolFraction.value pf) :> obj) } |> Map.ofSeq}
