namespace Archie.Base
open System

type SorterTestResultsDto = 
    {
        switchUsesDto:SwitchUsesDto;
        successfulSortCount:int;
        usedSwitchCount:int;
        usedStageCount:int
    }

module SorterTestResultsDto = 
    let fromDto (dto:SorterTestResultsDto) =
        result {
            let! switchUses = SwitchUsesDto.fromDto dto.switchUsesDto
            let! successfulSortCount = SortableCount.create "" dto.successfulSortCount
            let! usedSwitchCount = SwitchCount.create "" dto.usedSwitchCount
            let! usedStageCount = StageCount.create "" dto.usedStageCount

            return {
                SorterTestResults.switchUses=switchUses;
                successfulSortCount=successfulSortCount;
                usedSwitchCount=usedSwitchCount;
                usedStageCount=usedStageCount
            }
        }

    let toDto (sorterTestResults:SorterTestResults) =
        {
            SorterTestResultsDto.switchUsesDto = sorterTestResults.switchUses |> SwitchUsesDto.toDto
            successfulSortCount = (SortableCount.value sorterTestResults.successfulSortCount)
            usedSwitchCount = (SwitchCount.value sorterTestResults.usedSwitchCount)
            usedStageCount = (StageCount.value sorterTestResults.usedStageCount)
        }


type SorterPoolMemberDto = {
    id:Guid;
    poolMemberState:string; 
    birthDate:int; 
    parent:SorterPoolMemberDto option;
    sorterDto:SorterDto;
    poolMemberRank:int option;
    testResults:SorterTestResultsDto option;
    fitness:float option;
}

//module SorterPoolMemberDto =

//    let rec fromDto (dto:SorterPoolMemberDto) =
//        result {
//            let id = dto.id
//            let! pms = dto.poolMemberState |> PoolMemberState.fromDto
//            let! birthDate = GenerationNumber.create "" dto.birthDate
//            let! parent = match dto.parent with
//                          | Some dto -> (fromDto dto) |> Result.map Some
//                          | None -> None |> Ok
//            let! sorter = dto.sorterDto |> SorterDto.fromDto
//            let! poolMemberRank = match dto.poolMemberRank with
//                                  | Some pmr -> (PoolMemberRank.create "" pmr) 
//                                                 |> Result.map Some
//                                  | None -> None |> Ok
//            let! testResults = match dto.testResults with
//                               | Some tr -> (SorterTestResultsDto.fromDto tr)
//                                             |> Result.map Some
//                               | None -> None |> Ok

//            let! fitness =  match dto.fitness with
//                            | Some f -> (SorterFitness.create "" f) 
//                                         |> Result.map Some
//                            | None -> None |> Ok
//            return {
//                SorterPoolMember.id = id;
//                poolMemberState = pms
//                birthDate=birthDate;
//                parent=parent
//                sorter=sorter
//                poolMemberRank = poolMemberRank
//                testResults = testResults
//                fitness = fitness
//            }
//        }


//    let rec toDto (sorterPoolMember:SorterPoolMember) =
//        {
//            SorterPoolMemberDto.id = sorterPoolMember.id
//            poolMemberState = sorterPoolMember.poolMemberState 
//                                    |> PoolMemberState.toDto
//            birthDate = (GenerationNumber.value sorterPoolMember.birthDate)

//            parent = match sorterPoolMember.parent with
//                        | Some spm -> Some (toDto spm)
//                        | None -> None

//            sorterDto = sorterPoolMember.sorter |> SorterDto.toDto

//            poolMemberRank =  match sorterPoolMember.poolMemberRank with
//                                | Some r -> Some (PoolMemberRank.value r)
//                                | None -> None

//            testResults = match sorterPoolMember.testResults with
//                           | Some tr ->  Some (tr |> SorterTestResultsDto.toDto)
//                           | None -> None

//            fitness = match sorterPoolMember.fitness with
//                        | Some v -> Some (SorterFitness.value v)
//                        | None -> None
//        }











type SorterPoolMemberDto2 = {
    id:Guid;
    poolMemberState:string; 
    birthDate:int; 
    parent:Guid option;
    sorterDto:SorterDto;
    poolMemberRank:int option;
    testResults:SorterTestResultsDto option;
    fitness:float option;
}

module SorterPoolMemberDto2 =

    let rec fromDto (dto:SorterPoolMemberDto2) =
        result {
            let id = dto.id
            let! pms = dto.poolMemberState |> PoolMemberState.fromDto
            let! birthDate = GenerationNumber.create "" dto.birthDate
            let  parent = match dto.parent with
                          | Some guey -> Some (SorterPoolMemberRef.I guey)
                          | None -> None
            let! sorter = dto.sorterDto |> SorterDto.fromDto
            let! poolMemberRank = match dto.poolMemberRank with
                                  | Some pmr -> (PoolMemberRank.create "" pmr) 
                                                 |> Result.map Some
                                  | None -> None |> Ok
            let! testResults = match dto.testResults with
                               | Some tr -> (SorterTestResultsDto.fromDto tr)
                                             |> Result.map Some
                               | None -> None |> Ok

            let! fitness =  match dto.fitness with
                            | Some f -> (SorterFitness.create "" f) 
                                         |> Result.map Some
                            | None -> None |> Ok
            return {
                SorterPoolMember2.id = id;
                poolMemberState = pms
                birthDate=birthDate;
                parent= parent
                sorter=sorter
                poolMemberRank = poolMemberRank
                testResults = testResults
                fitness = fitness
            }
        }


    let toDto (sorterPoolMember:SorterPoolMember2) =
        {
            SorterPoolMemberDto2.id = sorterPoolMember.id
            poolMemberState = sorterPoolMember.poolMemberState 
                                    |> PoolMemberState.toDto
            birthDate = (GenerationNumber.value sorterPoolMember.birthDate)

            parent = match sorterPoolMember.parent with
                        | Some spm -> match spm with        
                                      | R s -> Some s.id
                                      | I g -> Some g
                        | None -> None

            sorterDto = sorterPoolMember.sorter |> SorterDto.toDto

            poolMemberRank =  match sorterPoolMember.poolMemberRank with
                                | Some r -> Some (PoolMemberRank.value r)
                                | None -> None

            testResults = match sorterPoolMember.testResults with
                           | Some tr ->  Some (tr |> SorterTestResultsDto.toDto)
                           | None -> None

            fitness = match sorterPoolMember.fitness with
                        | Some v -> Some (SorterFitness.value v)
                        | None -> None
        }























type SorterPool2Dto = {id:Guid; degree:int; sorterPoolMembers:SorterPoolMemberDto2[];}

module SorterPool2Dto =
   let fromDto (sorterPool2dto:SorterPool2Dto) = 
        result {
            let! degree = Degree.create "" sorterPool2dto.degree
            let! spms = sorterPool2dto.sorterPoolMembers
                        |> Array.map(SorterPoolMemberDto2.fromDto)
                        |> Array.toList |> Result.sequence

            return {
                        SorterPool2.id =sorterPool2dto.id;
                        degree = degree;
                        sorterPoolMembers = spms |> List.toArray
                   }
        }

   let toDto (sorterPool:SorterPool2) =
       {
            SorterPool2Dto.id = sorterPool.id;
            degree = (Degree.value sorterPool.degree);
            sorterPoolMembers = sorterPool.sorterPoolMembers
                                |> Array.map(fun spm -> SorterPoolMemberDto2.toDto spm)
       }

//type PoolUpdateParamsDto = {id:Guid;
//                            breederFrac:float; 
//                            runLength:int;
//                            sorterMutationTypeDto:SorterMutationTypeDto;
//                            poolCount:int;
//                            rngGenType:string;
//                            fitnessFuncDto:FitnessFuncDto
//                            legacyBias:float;
//                            winnerFrac:float }

//module PoolUpdateParamsDto =

    //let toDto (pup:PoolUpdateParams) =
    //    {
    //        breederFrac = PoolFraction.value pup.breederFrac;
    //        generationNumber = GenerationNumber.value pup.generationNumber;
    //        mutationTypeDto = pup.mutationType |> MutationTypeDto.toDto;
    //        poolCount = SorterCount.value pup.poolCount;
    //        rngGenDto= pup.rngGen |> RngGenDto.toDto;
    //        fitnessFuncDto = pup.fitnessFunc|> FitnessFuncDto.toDto;
    //        winnerFrac= PoolFraction.value pup.winnerFrac;
    //    }

    //let fromDto (dto:PoolUpdateParamsDto) =
    //    result {
    //            let! bf = PoolFraction.create "" dto.breederFrac
    //            let! gc = GenerationNumber.create "" dto.runLength
    //            let! mt = dto.sorterMutationTypeDto |> SorterMutationTypeDto.fromDto
    //            let! pc = SorterCount.create "" dto.poolCount
    //            let! rg = dto.rngGenType |> RngType.create
    //            let! ff = dto.fitnessFuncDto |> FitnessFuncDto.fromDto
    //            let! lb = SorterFitness.create "" dto.legacyBias
    //            let! wf = PoolFraction.create "" dto.winnerFrac
    //            return {
    //                     PoolUpdateParamsBnW.id = Guid.NewGuid();
    //                     breederFrac=bf;
    //                     fitnessFunc=ff;
    //                     runLength=gc;
    //                     legacyBias=lb;
    //                     mutationType=mt;
    //                     poolCount=pc;
    //                     rngType=rg;
    //                     winnerFrac=wf; }
    //           }


//type RndSorterParamsDto = 
//    {degree:int;
//     sorterCount:int;
//     rngGenDto:RngGenDto;
//     sorterLengthDto:SorterLengthDto}

//module RndSorterParamsDto = 

//    let toDto (rngt:RndSorterParams) =
//        {
//            degree = Degree.value rngt.degree;
//            sorterCount = SorterCount.value rngt.sorterCount;
//            rngGenDto = rngt.rngGen |> RngGenDto.toDto
//            sorterLengthDto = rngt.sorterLength |> SorterLengthDto.toDto;
//        }

//    let fromDto (dto:RndSorterParamsDto) =
//        result {
//            let! d = Degree.create "" dto.degree
//            let! sc = SorterCount.create "" dto.sorterCount
//            let! rngGen = dto.rngGenDto |> RngGenDto.fromDto
//            let! sorterLength = dto.sorterLengthDto |> SorterLengthDto.fromDto
//            return {
//                    RndSorterParams.degree = d;
//                    sorterCount = sc;
//                    rngGen = rngGen;
//                    sorterLength = sorterLength
//            }
//        }


type SorterPoolUpdateParamsDto = 
  {
      id: Guid;
      breederSelectorDto: PoolSelector2Dto;
      fitnessFuncDto: FitnessFuncDto;
      sorterMutatorDto: SorterMutationDto;
      sorterCount: int;
      winnerSelectorDto: PoolSelector2Dto;
  }

module SorterPoolUpdateParamsDto =
   let fromDto (sorterPoolUpdateParamsDto:SorterPoolUpdateParamsDto) = 
     result {
         let! fitnessFunc = FitnessFuncDto.fromDto sorterPoolUpdateParamsDto.fitnessFuncDto
         let! breederSelector = sorterPoolUpdateParamsDto.breederSelectorDto
                                |> PoolSelector2Dto.fromDto
         let! winnerSelector = sorterPoolUpdateParamsDto.winnerSelectorDto
                                |> PoolSelector2Dto.fromDto
         let! sorterCount = SorterCount.create "" sorterPoolUpdateParamsDto.sorterCount
         let! sorterMutator = sorterPoolUpdateParamsDto.sorterMutatorDto
                              |> SorterMutationDto.fromDto
         return {
                     SorterPoolUpdateParams.id = sorterPoolUpdateParamsDto.id;
                     SorterPoolUpdateParams.breederSelector = breederSelector;
                     SorterPoolUpdateParams.sorterCount = sorterCount;
                     SorterPoolUpdateParams.sorterMutator = sorterMutator;
                     SorterPoolUpdateParams.fitnessFunc = fitnessFunc
                     SorterPoolUpdateParams.winnerSelector = winnerSelector
                }
     }

   let toDto (sorterPoolUpdateParams:SorterPoolUpdateParams) =
      {
            SorterPoolUpdateParamsDto.id = sorterPoolUpdateParams.id;
            SorterPoolUpdateParamsDto.breederSelectorDto = sorterPoolUpdateParams.breederSelector |> PoolSelector2Dto.toDto;
            SorterPoolUpdateParamsDto.sorterCount = sorterPoolUpdateParams.sorterCount |> SorterCount.value
            SorterPoolUpdateParamsDto.sorterMutatorDto = sorterPoolUpdateParams.sorterMutator |> SorterMutationDto.toDto;
            SorterPoolUpdateParamsDto.fitnessFuncDto = sorterPoolUpdateParams.fitnessFunc |> FitnessFuncDto.toDto;
            SorterPoolUpdateParamsDto.winnerSelectorDto = sorterPoolUpdateParams.winnerSelector |> PoolSelector2Dto.toDto;
       }

    
type SorterPoolRunParamsDto = 
    {
        id:Guid;
        startingSorterPoolDto:SorterPool2Dto;
        runLength:int;
        sorterPoolUpdateParamsDto:SorterPoolUpdateParamsDto;
        rngGenDtos:Map<string, RngGenDto>
    }
        
module SorterPoolRunParamsDto =
   let fromDto (sorterPoolRunParamsDto:SorterPoolRunParamsDto) = 
     result {
         let! startingSorterPool = SorterPool2Dto.fromDto sorterPoolRunParamsDto.startingSorterPoolDto
         let! runLength = sorterPoolRunParamsDto.runLength |> GenerationNumber.create ""
         let! sorterPoolUpdateParams = sorterPoolRunParamsDto.sorterPoolUpdateParamsDto
                                        |> SorterPoolUpdateParamsDto.fromDto
         let! rngGens = sorterPoolRunParamsDto.rngGenDtos
                        |> Map.toList 
                        |> List.map(fun (k,v) ->
                                v |> RngGenDto.fromDto |> Result.map(fun r -> (k,r)))
                        |> Result.sequence
                        |> Result.map(fun l -> Map.ofList l)
         return {
                     SorterPoolRunParams.id = sorterPoolRunParamsDto.id;
                     SorterPoolRunParams.startingSorterPool = startingSorterPool;
                     SorterPoolRunParams.runLength = runLength;
                     SorterPoolRunParams.sorterPoolUpdateParams = sorterPoolUpdateParams;
                     SorterPoolRunParams.rngGens = rngGens
                }
     }

   let toDto (sorterPoolRunParams:SorterPoolRunParams) =
      {
            SorterPoolRunParamsDto.id = sorterPoolRunParams.id;
            SorterPoolRunParamsDto.startingSorterPoolDto = sorterPoolRunParams.startingSorterPool |> SorterPool2Dto.toDto;
            SorterPoolRunParamsDto.runLength = sorterPoolRunParams.runLength |> GenerationNumber.value
            SorterPoolRunParamsDto.sorterPoolUpdateParamsDto = sorterPoolRunParams.sorterPoolUpdateParams |> SorterPoolUpdateParamsDto.toDto;
            SorterPoolRunParamsDto.rngGenDtos = sorterPoolRunParams.rngGens  |> Map.map(fun k v -> v |> RngGenDto.toDto)
       }
       