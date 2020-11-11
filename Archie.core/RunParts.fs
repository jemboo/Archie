namespace Archie.Base
open Microsoft.FSharp.Collections
open System

module ParamUtils =
    let SplitPoolGenBnWFrac (poolSize:int) (dex:int) =
        match (dex % 6) with
        | 0 -> GenerationNumber.fromInt (poolSize/2), 2
        | 1 -> GenerationNumber.fromInt (poolSize/4), 4
        | 2 -> GenerationNumber.fromInt (poolSize/8), 8
        | 3 -> GenerationNumber.fromInt (poolSize/2), 2
        | 4 -> GenerationNumber.fromInt (poolSize/4), 4
        | _ -> GenerationNumber.fromInt (poolSize/8), 8

    let SplitPoolGenMut (poolSize:int) (dex:int) =
        match (dex % 6) with
        | 0 -> GenerationNumber.fromInt (poolSize/2), SorterCount.fromInt 2, SorterMutationType.Switch (MutationRate.fromFloat 0.01)
        | 1 -> GenerationNumber.fromInt (poolSize/2), SorterCount.fromInt 2, SorterMutationType.Switch (MutationRate.fromFloat 0.015)
        | 2 -> GenerationNumber.fromInt (poolSize/2), SorterCount.fromInt 2, SorterMutationType.Switch (MutationRate.fromFloat 0.02)
        | 3 -> GenerationNumber.fromInt (poolSize/2), SorterCount.fromInt 2, SorterMutationType.Stage (MutationRate.fromFloat 0.08)
        | 4 -> GenerationNumber.fromInt (poolSize/2), SorterCount.fromInt 2, SorterMutationType.Stage (MutationRate.fromFloat 0.12)
        | _ -> GenerationNumber.fromInt (poolSize/2), SorterCount.fromInt 2, SorterMutationType.Stage (MutationRate.fromFloat 0.16)


    let SplitPoolGenMut2 (poolSize:int) (dex:int) =
        match (dex % 3) with
        | 0 -> GenerationNumber.fromInt (poolSize/16), SorterCount.fromInt 16, SorterMutationType.Stage (MutationRate.fromFloat 0.12)
        | 1 -> GenerationNumber.fromInt (poolSize/8), SorterCount.fromInt 8, SorterMutationType.Stage (MutationRate.fromFloat 0.10)
        | _ -> GenerationNumber.fromInt (poolSize/4), SorterCount.fromInt 4, SorterMutationType.Stage (MutationRate.fromFloat 0.08)



    let SplitGenMtMr (poolGenCount:PoolGenCount) (dex:int) =
        match (dex % 6) with
        | 0 -> (PoolGenCount.value poolGenCount) / 2, SorterCount.fromInt 2, SorterMutationType.Switch (MutationRate.fromFloat 0.01)
        | 1 -> (PoolGenCount.value poolGenCount) / 2, SorterCount.fromInt 2, SorterMutationType.Switch (MutationRate.fromFloat 0.015)
        | 2 -> (PoolGenCount.value poolGenCount) / 2, SorterCount.fromInt 2, SorterMutationType.Switch (MutationRate.fromFloat 0.02)
        | 3 -> (PoolGenCount.value poolGenCount) / 2, SorterCount.fromInt 2, SorterMutationType.Stage (MutationRate.fromFloat 0.08)
        | 4 -> (PoolGenCount.value poolGenCount) / 2, SorterCount.fromInt 2, SorterMutationType.Stage (MutationRate.fromFloat 0.12)
        | _ -> (PoolGenCount.value poolGenCount) / 2, SorterCount.fromInt 2, SorterMutationType.Stage (MutationRate.fromFloat 0.16)


    let SplitPoolGenMr (poolGenCount:PoolGenCount) (dex:int) =
          match (dex % 3) with
          | 0 -> (PoolGenCount.value poolGenCount) / 16, SorterCount.fromInt 16, SorterMutationType.Stage (MutationRate.fromFloat 0.12)
          | 1 -> (PoolGenCount.value poolGenCount) / 8, SorterCount.fromInt 8, SorterMutationType.Stage (MutationRate.fromFloat 0.10)
          | _ -> (PoolGenCount.value poolGenCount) / 4, SorterCount.fromInt 4, SorterMutationType.Stage (MutationRate.fromFloat 0.08)


    let SplitPoolMr (poolGenCount:PoolGenCount) (dex:int) =
          match (dex % 3) with
          | 0 -> (PoolGenCount.value poolGenCount) / 2, SorterCount.fromInt 2, SorterMutationType.Stage (MutationRate.fromFloat 0.12)
          | 1 -> (PoolGenCount.value poolGenCount) / 2, SorterCount.fromInt 2, SorterMutationType.Stage (MutationRate.fromFloat 0.10)
          | _ -> (PoolGenCount.value poolGenCount) / 2, SorterCount.fromInt 2, SorterMutationType.Stage (MutationRate.fromFloat 0.08)



type PoolUpdateParamsBnW = 
    {
        id:Guid;
        breederFrac:PoolFraction;
        fitnessFunc:FitnessFunc;
        runLength:GenerationNumber;
        legacyBias:SorterFitness;
        mutationType:SorterMutationType;
        poolCount:SorterCount;
        rngType:RngType;
        winnerFrac:PoolFraction;
    }

module PoolUpdateParamsBnW =

    let headers =
        [|"id"; "breederFrac"; "winnerFrac"; "generation"; "mutationType"; 
          "mutationRate"; 
          "runSeed"; "ff_cat"; "ff_args"; "legacyBias"; "poolCount"|]


    let report (pup:PoolUpdateParamsBnW) =
        let mtDto = SorterMutationTypeDto.toDto pup.mutationType
        let ffreport = FitnessFuncCat.report pup.fitnessFunc.cat
        [|sprintf "%A" pup.id;
          sprintf "%.3f" (PoolFraction.value pup.breederFrac);
          sprintf "%.3f" (PoolFraction.value pup.winnerFrac);
          sprintf "%d" (GenerationNumber.value pup.runLength);
          sprintf "%s" mtDto.mType;
          sprintf "%.4f" mtDto.rate;
          sprintf "%s" (fst ffreport);
          sprintf "%s" (snd ffreport);
          sprintf "%.4f" (SorterFitness.value pup.legacyBias);
          sprintf "%d" (SorterCount.value pup.poolCount)|]


    let mutator prams (rando:IRando) =
        Sorter.mutate prams.mutationType rando


    let SplitPoolGenBnWFrac (poolSize:int) (dex:int) =
        match (dex % 6) with
        | 0 -> GenerationNumber.fromInt (poolSize/2), SorterCount.fromInt 2, (PoolFraction.fromFloat 0.5), (PoolFraction.fromFloat 0.5)
        | 1 -> GenerationNumber.fromInt (poolSize/4), SorterCount.fromInt 4, (PoolFraction.fromFloat 0.25), (PoolFraction.fromFloat 0.25)
        | 2 -> GenerationNumber.fromInt (poolSize/8), SorterCount.fromInt 8, (PoolFraction.fromFloat 0.125), (PoolFraction.fromFloat 0.125)
        | 3 -> GenerationNumber.fromInt (poolSize/2), SorterCount.fromInt 2, (PoolFraction.fromFloat 0.5), (PoolFraction.fromFloat 0.5)
        | 4 -> GenerationNumber.fromInt (poolSize/4), SorterCount.fromInt 4, (PoolFraction.fromFloat 0.5), (PoolFraction.fromFloat 0.25)
        | _ -> GenerationNumber.fromInt (poolSize/8), SorterCount.fromInt 8, (PoolFraction.fromFloat 0.5), (PoolFraction.fromFloat 0.125)


    let Params10 (rngGen:RngGen) (poolSize:int) =
        let pm dex gu =
            let gc, pc, bFrac, wFrac  = SplitPoolGenBnWFrac poolSize dex
            {
                id = gu;
                breederFrac = bFrac;
                runLength = gc;
                legacyBias = SorterFitness.fromFloat 0.00;
                mutationType = SorterMutationType.Switch (MutationRate.fromFloat 0.02);
                poolCount = pc;
                rngType=RngType.Lcg;
                fitnessFunc = FitnessFunc.standardSwitch;
                winnerFrac = wFrac;
            }
        RandoCollections.IndexedGuidGen rngGen |> Seq.map(fun (dex, gu) -> pm dex gu)


    let SplitPoolGenMff (dex:int) =
        let cycleG1 = GenerationNumber.fromInt 200
        let cycleG2 = GenerationNumber.fromInt 600
        match (dex % 3) with
        | 0 -> FitnessFunc.altSwitchAndStage cycleG1
        | 1 -> FitnessFunc.altSwitchAndStage cycleG2
        | _ -> FitnessFunc.standardSwitch


    let ParamsMr (rngGen:RngGen) (poolGenCount:PoolGenCount) =
        let pm dex gu =
            let gc, pc, mut = ParamUtils.SplitPoolMr poolGenCount dex
            {
                id = gu;
                breederFrac=(PoolFraction.fromFloat 0.5);
                runLength=GenerationNumber.fromInt gc;
                mutationType=mut;
                legacyBias = SorterFitness.fromFloat 0.0;
                poolCount = pc;
                rngType=RngType.Lcg;
                fitnessFunc=FitnessFunc.standardSwitch;
                winnerFrac=(PoolFraction.fromFloat 0.5);
            }
        RandoCollections.IndexedGuidGen rngGen |> Seq.map(fun (dex, gu) -> pm dex gu)


    let ParamsMff (rngGen:RngGen) (poolGenCount:PoolGenCount) =
        let pm dex gu =
            let ff = SplitPoolGenMff dex
            {
                id = gu;
                breederFrac=(PoolFraction.fromFloat 0.5);
                runLength=GenerationNumber.fromInt (PoolGenCount.value poolGenCount / 2);
                legacyBias = SorterFitness.fromFloat 0.0;
                mutationType=SorterMutationType.Stage (MutationRate.fromFloat 0.10);
                poolCount=SorterCount.fromInt 2;
                rngType=RngType.Lcg;
                fitnessFunc=ff;
                winnerFrac=(PoolFraction.fromFloat 0.5);
            }
        RandoCollections.IndexedGuidGen rngGen |> Seq.map(fun (dex, gu) -> pm dex gu)
        

    //let ParamsSnS (rngGen:RngGen) (poolGenCount:PoolGenCount) (poolCount:SorterCount)
    //              (sorterMutationType:SorterMutationType) (legacyBias:SorterFitness)
    //              (mcBase:float) (mcInc:float) (usageRbase:float) (usageRinc:float) =
    //    let pm dex gu =
    //        let ff = SplitStageAndSuccess dex mcBase mcInc usageRbase usageRinc
    //        let poolFrac = 1.0 / float (SorterCount.value poolCount) |> PoolFraction.fromFloat;
    //        {
    //            id = gu;
    //            breederFrac = poolFrac;
    //            runLength = GenerationNumber.fromInt (PoolGenCount.value poolGenCount / 2);
    //            legacyBias = legacyBias;
    //            mutationType = sorterMutationType;
    //            poolCount = poolCount;
    //            rngType = RngType.Lcg;
    //            fitnessFunc = ff;
    //            winnerFrac = poolFrac;
    //        }
    //    RandoCollections.IndexedGuidGen rngGen |> Seq.map(fun (dex, gu) -> pm dex gu)

