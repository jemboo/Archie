namespace Archie.Base

open System

type FitnessFuncParam =
     | NoParam
     | SwitchOnly of SwitchCount
     | StageOnly of StageCount
     | Gen of GenerationNumber
     | Pool of GenerationNumber*SorterPoolMember[]

type FitnessFunc2 = private FitnessFunc2 of (FitnessFuncParam->SorterTestResults->SorterFitness)

module FitnessFunc2 =
    
    let create ff =
        (FitnessFunc2 ff)

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


type FitnessFunc2DtoCat =
     | StandardSwitch
     | StandardStage
     | AltSwitchAndStage
     | AltSwitchStageAndSuccess

type FitnessFunc2Dto = {cat:FitnessFunc2DtoCat; prams:Map<string, obj>}

module FitnessFunc2Dto =
    
    let standardSwitchDto = 
        {cat=FitnessFunc2DtoCat.StandardSwitch; prams = Seq.empty |> Map.ofSeq}

    let standardStageDto = 
        {cat=FitnessFunc2DtoCat.StandardStage;  prams = Seq.empty |> Map.ofSeq}

    let altSwitchAndStageDto (cycleG:GenerationNumber) =  
        {cat=FitnessFunc2DtoCat.AltSwitchAndStage; 
         prams = seq { ("cycleG", (GenerationNumber.value cycleG) :> obj) } |> Map.ofSeq}
       
    let altSwitchStageAndSuccessDto (cycleG:GenerationNumber) = 
        {cat=FitnessFunc2DtoCat.AltSwitchStageAndSuccess; 
         prams = seq { ("cycleG", (GenerationNumber.value cycleG) :> obj) } |> Map.ofSeq}

    let fromDto (fitnessFunc2Dto:FitnessFunc2Dto) =
        match fitnessFunc2Dto.cat with
        | StandardSwitch -> FitnessFunc2.standardSwitch |> Ok
        | StandardStage -> FitnessFunc2.standardStage |> Ok
        | AltSwitchAndStage ->  
           result {
                    let! g = GenerationNumber.fromKey fitnessFunc2Dto.prams "cycleG"
                    return FitnessFunc2.altSwitchAndStage g
                  }
        | AltSwitchStageAndSuccess ->
           result {
                     let! g = GenerationNumber.fromKey fitnessFunc2Dto.prams "cycleG"
                     return FitnessFunc2.altSwitchStageAndSuccess g
                  }


     