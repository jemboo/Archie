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
        parent:SorterPoolMember option;
        sorter:Sorter;
        poolMemberRank:PoolMemberRank option;
        testResults:SorterTestResults option;
        fitness:SorterFitness option;
    }


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
       | Legacy | Evaluated -> pm.fitness |> Option.get
       | Measured -> failwith "cannot getFitness from Measured"
       | Initiate _ -> failwith "cannot getFitness from Initiate"
       | Archived _ -> failwith "cannot getFitness from Archived"
       | Root _ -> failwith "cannot getFitness from None"


    let isTopRanked (pm:SorterPoolMember) =
       match pm.poolMemberRank with
       | Some r -> r = (PoolMemberRank.fromInt 1)
       | None -> false


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
                       (w, (SorterFitness.value (SorterPoolMember.getFitness w))))
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
