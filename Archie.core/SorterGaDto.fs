namespace Archie.Base

type FitnessFuncDto = {cat:string; args:string;}
module FitnessFuncDto =
    let toDto (ff:FitnessFunc) =
        {
            cat=ff.funcType;
            args = sprintf "%float" (ff.funcParam :?> float)
        }

    let fromDto (dto:FitnessFuncDto) =
        result {
                match dto.cat with
                | "Switch" -> let! off = ParseUtils.StringToOneFloat dto.args
                              return FitnessFunc.standardSwitch off
                | "Stage" ->  let! off = ParseUtils.StringToOneFloat dto.args
                              return FitnessFunc.standardStage off
                | _ ->        let! res =  Error (sprintf "no match for FitnessFunc: %s" dto.cat)
                              return res
            }


type PoolUpdateParamsDto = {breederFrac:float; 
                            generationCount:int;
                            mutationTypeDto:MutationTypeDto;
                            poolCount:int;
                            rngGenDto:RngGenDto;
                            fitnessFuncDto:FitnessFuncDto
                            winnerFrac:float }

module PoolUpdateParamsDto =

    let toDto (pup:PoolUpdateParams) =
        {
            breederFrac = PoolFraction.value pup.breederFrac;
            generationCount = GenerationCount.value pup.generationCount;
            mutationTypeDto = pup.mutationType |> MutationTypeDto.toDto;
            poolCount = SorterCount.value pup.poolCount;
            rngGenDto= pup.rngGen |> RngGenDto.toDto;
            fitnessFuncDto = pup.fitnessFunc|> FitnessFuncDto.toDto;
            winnerFrac= PoolFraction.value pup.winnerFrac;
        }

    let fromDto (dto:PoolUpdateParamsDto) =
        result {
                let! bf = PoolFraction.create "" dto.breederFrac
                let! gc = GenerationCount.create "" dto.generationCount
                let! mt = dto.mutationTypeDto |> MutationTypeDto.fromDto
                let! pc = SorterCount.create "" dto.poolCount
                let! rg = dto.rngGenDto |> RngGenDto.fromDto
                let! ff = dto.fitnessFuncDto |> FitnessFuncDto.fromDto
                let! wf = PoolFraction.create "" dto.winnerFrac
                return {
                    breederFrac=bf;
                    generationCount=gc;
                    mutationType=mt;
                    poolCount=pc;
                    rngGen=rg;
                    fitnessFunc=ff;
                    winnerFrac=wf;}
          }


type RndSorterPoolParamsDto = 
    {degree:int;
     sorterCount:int;
     rngGenDto:RngGenDto;
     rndSorterGenDto:RndSorterGenDto}

module RndSorterPoolParamsDto = 
    let toDto (rngt:RndSorterPoolParams) =
        {
            degree = Degree.value rngt.degree;
            sorterCount = SorterCount.value rngt.sorterCount;
            rngGenDto = rngt.rngGen |> RngGenDto.toDto
            rndSorterGenDto = rngt.rndSorterGen |> RndSorterGenDto.toDto;
        }

    let fromDto (dto:RndSorterPoolParamsDto) =
        result {
            let! d = Degree.create "" dto.degree
            let! sc = SorterCount.create "" dto.sorterCount
            let! rngGen = dto.rngGenDto |> RngGenDto.fromDto
            let! rndSorterGen = dto.rndSorterGenDto |> RndSorterGenDto.fromDto
            return {
                    RndSorterPoolParams.degree = d;
                    RndSorterPoolParams.sorterCount = sc;
                    RndSorterPoolParams.rngGen = rngGen;
                    RndSorterPoolParams.rndSorterGen = rndSorterGen
            }
        }
