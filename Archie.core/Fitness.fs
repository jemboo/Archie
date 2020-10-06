namespace Archie.Base

open System

type PoolMemberState =
    | Root
    | Initiate
    | Measured
    | Evaluated
    | Legacy
    | Archived


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

module FitnessFunc = 

    let standardSwitch = 
        {FitnessFunc.cat=FitnessFuncCat.StandardSwitch; func=FF.standardSwitch}

    let standardStage = 
        {FitnessFunc.cat=FitnessFuncCat.StandardStage; func=FF.standardSwitch}

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

    let fromDto (FitnessFuncDto:FitnessFuncDto) =
        match FitnessFuncDto.cat with
        | "StandardSwitch" -> FitnessFunc.standardSwitch |> Ok
        | "StandardStage" -> FitnessFunc.standardStage |> Ok
        | "AltSwitchAndStage" ->  
           result {
                    let! g = GenerationNumber.fromKey FitnessFuncDto.prams "cycleG"
                    return FitnessFunc.altSwitchAndStage g
                  }
        | "AltSwitchStageAndSuccess" ->
           result {
                     let! g = GenerationNumber.fromKey FitnessFuncDto.prams "cycleG"
                     return FitnessFunc.altSwitchStageAndSuccess g
                  }


     