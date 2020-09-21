namespace Archie.Base

open System

type FitnessFunc = {cat:string; args:string; func:obj option->SorterTestResults->SorterFitness}
module FitnessFunc =

    let mesa (offset:float) (value:int) =
        let fv = (float value)
        match fv with
        | v when v > offset -> 1.0 / (v - offset)
        | v -> 0.0 // 1.0 / (offset + 1.0 - v)

    let standardSwitch offset = 
        { cat = (sprintf "switch");
          args = (sprintf "%f" offset);
          func = (fun o r -> SorterFitness.fromFloat (mesa offset (SwitchCount.value r.usedSwitchCount)))}

    let standardStage offset = 
        { cat = (sprintf "stage");
          args = (sprintf "%f" offset);
          func = (fun o r -> SorterFitness.fromFloat (mesa offset (SwitchCount.value r.usedSwitchCount)))}

    let altSwitchAndStage offsetT offsetW (cycleG:GenerationNumber) = 
        let ff (p:obj option) r =
            let g = p |> Option.get :?> GenerationNumber
            let phase = (GenerationNumber.value g) % ((GenerationNumber.value cycleG) * 2)
            if (phase < (GenerationNumber.value cycleG)) then
               SorterFitness.fromFloat (mesa offsetT (StageCount.value r.stageUseCount))
            else
               SorterFitness.fromFloat (mesa offsetW (SwitchCount.value r.usedSwitchCount))
        { cat = (sprintf "WaltT");
          args = (sprintf "%f %f %d" offsetT offsetW (GenerationNumber.value cycleG));
          func = ff }

    let switchAndSuccess (maxCoverage:float) (usageR:float) = 
        let sas (p:obj option) r  =
            let g = p |> Option.get :?> GenerationNumber
            let cov = (float (SortableCount.value r.successfulSortCount)) / 
                           (maxCoverage - float (SortableCount.value r.successfulSortCount))
            let tweak = (20000.0 - float (GenerationNumber.value g)) / 10000.0
            let sw = usageR * ( 1.0 + ( Math.Cos((float (SwitchCount.value r.usedSwitchCount) / 34.0) ) ))
            SorterFitness.fromFloat (cov + sw*tweak)
        { cat = (sprintf "wNs");
          args = (sprintf "%f %.4f" maxCoverage usageR);
          func = sas }

    let stageAndSuccess (maxCoverage:float) (usageR:float) = 
        let cosy v =
            if v < Math.PI / 2.0 then 
                Math.Cos(v) 
            else 
                (Math.PI / 2.0) - v 
        let sas o r =
            //let nuCov = maxCoverage + 20000.0 - (float (GenerationNumber.value g))
            //let cov = (float (SortableCount.value r.successfulSortCount)) / 
            //              (nuCov - float (SortableCount.value r.successfulSortCount))
            let cov = (float (SortableCount.value r.successfulSortCount)) / 
                          (maxCoverage - float (SortableCount.value r.successfulSortCount))
            let st = usageR * ( 1.0 + (cosy((float (StageCount.value r.stageUseCount) / 6.0) ) ))     //Degree12:9
            let sw = 0.5 * usageR * ( 1.0 + (cosy((float (SwitchCount.value r.usedSwitchCount) / 34.0) ) ))  //Degree12:39
            SorterFitness.fromFloat (cov + sw + st)
        { cat = (sprintf "tNs");
          args = (sprintf "%f %.4f" maxCoverage usageR);
          func = sas }

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



// Params for Random walk process
type RwUpdateParams = 
    {generationNumber:GenerationNumber;
    mutationType:SorterMutationType;
    poolCount:SorterCount;
    rngGen:RngGen;
    fitnessFunc:FitnessFunc;}


module RwUpdateParams =

  let Params10 (rngGen:RngGen) (poolSize:int) =
    let pm rg dex =
        let gc, pc = ParamUtils.SplitPoolGenBnWFrac poolSize dex
        {
            generationNumber = gc;
            mutationType=SorterMutationType.Switch (MutationRate.fromFloat 0.02);
            poolCount=SorterCount.fromInt pc;
            rngGen=rg;
            fitnessFunc=FitnessFunc.standardSwitch 4.0;
        }
    RandoCollections.IndexedSeedGen rngGen |> Seq.map(fun (dex, rg) -> pm rg dex)

  let ParamsM (rngGen:RngGen) (poolCount:int) =
      let pm rg dex =
          let gc, pc, mut = ParamUtils.SplitPoolGenMut2 poolCount dex
          {
              generationNumber = gc;
              mutationType = mut;
              poolCount = pc;
              rngGen = rg;
              fitnessFunc=FitnessFunc.standardSwitch 4.0 ;
          }
      RandoCollections.IndexedSeedGen rngGen |> Seq.map(fun (dex, rg) -> pm rg dex)


type PoolUpdateParamsBnW = 
        {breederFrac:PoolFraction;
        fitnessFunc:FitnessFunc;
        generationNumber:GenerationNumber;
        legacyBias:SorterFitness;
        mutationType:SorterMutationType;
        poolCount:SorterCount;
        rngGen:RngGen;
        winnerFrac:PoolFraction;}


module PoolUpdateParamsBnW =

    let headers =
        [|"breederFrac"; "winnerFrac"; "generation"; "mutationType"; "mutationRate"; 
          "runSeed"; "ff_cat"; "ff_args"; "legacyBias"; "poolCount"|]

    let report (pup:PoolUpdateParamsBnW) =
        let rgDto = RngGenDto.toDto pup.rngGen
        let mtDto = SorterMutationTypeDto.toDto pup.mutationType
        [|sprintf "%.3f" (PoolFraction.value pup.breederFrac);
          sprintf "%.3f" (PoolFraction.value pup.winnerFrac);
          sprintf "%d" (GenerationNumber.value pup.generationNumber);
          sprintf "%s" mtDto.mType;
          sprintf "%.4f" mtDto.rate;
          sprintf "%d" rgDto.seed;
          sprintf "%s" pup.fitnessFunc.cat;
          sprintf "%s" pup.fitnessFunc.args;
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
        let pm rg dex =
            let gc, pc, bFrac, wFrac  = SplitPoolGenBnWFrac poolSize dex
            {
                breederFrac = bFrac;
                generationNumber = gc;
                legacyBias = SorterFitness.fromFloat 0.00;
                mutationType = SorterMutationType.Switch (MutationRate.fromFloat 0.02);
                poolCount = pc;
                rngGen = rg;
                fitnessFunc = FitnessFunc.standardSwitch 4.0;
                winnerFrac = wFrac;
            }
        RandoCollections.IndexedSeedGen rngGen |> Seq.map(fun (dex, rg) -> pm rg dex)


    let SplitPoolGenMff (dex:int) =
        let offsetT = 3.0
        let offsetW = 20.0
        let cycleG1 = GenerationNumber.fromInt 200
        let cycleG2 = GenerationNumber.fromInt 600
        match (dex % 3) with
        | 0 -> FitnessFunc.altSwitchAndStage offsetT offsetW cycleG1
        | 1 -> FitnessFunc.altSwitchAndStage offsetT offsetW cycleG2
        | _ -> FitnessFunc.standardSwitch offsetW


    let SplitSwitchAndSuccess (dex:int) =
        match (dex % 3) with
        | 0 -> FitnessFunc.switchAndSuccess (16384.0 + 120.0) (2.0)
        | 1 -> FitnessFunc.switchAndSuccess (16384.0 + 120.0) (3.5)
        | _ -> FitnessFunc.switchAndSuccess (16384.0 + 120.0) (5.0)


    let SplitStageAndSuccess (dex:int) (mcBase:float) (mcInc:float) (usageRbase:float) (usageRinc:float) =
        match (dex % 3) with
        | 0 -> FitnessFunc.stageAndSuccess (4096.0 + mcBase) (usageRbase)
        | 1 -> FitnessFunc.stageAndSuccess (4096.0 + mcBase + mcInc ) (usageRbase + usageRinc)
        | _ -> FitnessFunc.stageAndSuccess (4096.0 + mcBase + mcInc + mcInc) (usageRbase + usageRinc + usageRinc)

    //let SplitStageAndSuccess (dex:int) =
    //    match (dex % 3) with
    //    | 0 -> FitnessFunc.stageAndSuccess (16384.0 + 20120.0) (2.0)
    //    | 1 -> FitnessFunc.stageAndSuccess (16384.0 + 20120.0) (3.5)
    //    | _ -> FitnessFunc.stageAndSuccess (16384.0 + 20120.0) (5.0)


    let ParamsMr (rngGen:RngGen) (poolGenCount:PoolGenCount) =
        let pm rg dex =
            let gc, pc, mut = ParamUtils.SplitPoolMr poolGenCount dex
            {
                breederFrac=(PoolFraction.fromFloat 0.5);
                generationNumber=GenerationNumber.fromInt gc;
                mutationType=mut;
                legacyBias = SorterFitness.fromFloat 0.0;
                poolCount = pc;
                rngGen=rg;
                fitnessFunc=FitnessFunc.standardSwitch 4.0;
                winnerFrac=(PoolFraction.fromFloat 0.5);
            }
        RandoCollections.IndexedSeedGen rngGen |> Seq.map(fun (dex, rg) -> pm rg dex)


    let ParamsMff (rngGen:RngGen) (poolGenCount:PoolGenCount) =
        let pm rg dex =
            let ff = SplitPoolGenMff dex
            {
                breederFrac=(PoolFraction.fromFloat 0.5);
                generationNumber=GenerationNumber.fromInt (PoolGenCount.value poolGenCount / 2);
                legacyBias = SorterFitness.fromFloat 0.0;
                mutationType=SorterMutationType.Stage (MutationRate.fromFloat 0.10);
                poolCount=SorterCount.fromInt 2;
                rngGen=rg;
                fitnessFunc=ff;
                winnerFrac=(PoolFraction.fromFloat 0.5);
            }
        RandoCollections.IndexedSeedGen rngGen |> Seq.map(fun (dex, rg) -> pm rg dex)
        

    let ParamsSnS (rngGen:RngGen) (poolGenCount:PoolGenCount) (poolCount:SorterCount)
                  (sorterMutationType:SorterMutationType) (legacyBias:SorterFitness)
                  (mcBase:float) (mcInc:float) (usageRbase:float) (usageRinc:float) =
        let pm rg dex =
            let ff = SplitStageAndSuccess dex mcBase mcInc usageRbase usageRinc
            let poolFrac = 1.0 / float (SorterCount.value poolCount) |> PoolFraction.fromFloat;
            {
                breederFrac = poolFrac;
                generationNumber=GenerationNumber.fromInt (PoolGenCount.value poolGenCount / 2);
                legacyBias = legacyBias;
                mutationType=sorterMutationType;
                poolCount=poolCount;
                rngGen=rg;
                fitnessFunc=ff;
                winnerFrac= poolFrac;
            }
        RandoCollections.IndexedSeedGen rngGen |> Seq.map(fun (dex, rg) -> pm rg dex)


type RndSorterParams = 
    {degree:Degree;
     sorterCount:SorterCount;
     rngGen:RngGen;
     sorterLength:SorterLength}


module RndSorterParams =

    let Make (degree:Degree) (sorterCount:SorterCount) 
             (rngGen:RngGen) (wOrT:SwitchOrStage) =
    
        result {
            let sorterLength = SorterLength.to999Sucessful degree wOrT
            return 
                {RndSorterParams.degree=degree;
                sorterCount=sorterCount;
                rngGen=rngGen;
                sorterLength = sorterLength}
        }


