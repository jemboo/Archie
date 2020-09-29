namespace Archie.Base

open System

//type FitnessFuncDto = {cat:string; args:string;}
//module FitnessFuncDto =
//    //let toDto (ff:FitnessFunc) =
//    //    {
//    //        cat=ff.funcType;
//    //        args = sprintf "%float" (ff.funcParam :?> float)
//    //    }

//    let fromDto (dto:FitnessFuncDto) =
//        result {
//                match dto.cat with
//                | "Switch" -> let! off = ParseUtils.StringToOneFloat dto.args
//                              return FitnessFunc.standardSwitch off
//                | "Stage" ->  let! off = ParseUtils.StringToOneFloat dto.args
//                              return FitnessFunc.standardStage off
//                | _ ->        let! res =  Error (sprintf "no match for FitnessFunc: %s" dto.cat)
//                              return res
//            }


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