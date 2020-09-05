namespace Archie.Base

type FitnessFunc ={funcType:string; funcParam:obj; fitnessFunc:obj->SorterFitness}
module FitnessFunc =
    let sorterFitness (offset:float) (value:int) =
        let fv = (float value)
        match fv with
        | v when v > offset -> SorterFitness.create "" (1.0 / (v - offset)) 
                                    |> Result.ExtractOrThrow
        | v -> SorterFitness.create "" (1.0 / (offset + 1.0 - v)) 
                                    |> Result.ExtractOrThrow

    let standardSwitch offset = 
       {
            funcType="Switch";
            funcParam=offset;
            fitnessFunc = fun fv -> sorterFitness offset (fv :?> int);
       }

    let standardStage offset = 
        {
            funcType="Stage";
            funcParam=offset;
            fitnessFunc = fun fv -> sorterFitness offset (fv :?> int)
        }


type RwUpdateParams = 
    {generationNumber:GenerationNumber;
    mutationType:MutationType;
    poolCount:SorterCount;
    rngGen:RngGen;
    fitnessFunc:FitnessFunc;}


module RwUpdateParams =

  let MutationTypes =
      let fr r = MutationRate.create "" r |> Result.ExtractOrThrow
      let fts r = MutationType.Stage (fr r)
      let ftw r = MutationType.Switch (fr r)
      let svs = seq {0.06 .. 0.02 .. 0.16} |> Seq.map(fts)
      let svw = seq {0.06 .. 0.02 .. 0.16} |> Seq.map(ftw)
      svs |> Seq.append svw |> Seq.toArray


  let SplitPoolGen (poolSize:int) (dex:int) =
    match (dex % 3) with
    | 0 -> poolSize/2, 2
    | 1 -> poolSize/4, 4
    | _ -> poolSize/8, 8
 

  let SplitPoolGenBnWFrac (poolSize:int) (dex:int) =
      match (dex % 6) with
      | 0 -> poolSize/2, 2
      | 1 -> poolSize/4, 4
      | 2 -> poolSize/8, 8
      | 3 -> poolSize/2, 2
      | 4 -> poolSize/4, 4
      | _ -> poolSize/8, 8


  let Params10 (rngGen:RngGen) (poolSize:int) =
    let pm rg dex =
        let gc, pc = SplitPoolGenBnWFrac poolSize dex
        {
            generationNumber=GenerationNumber.fromInt gc;
            mutationType=MutationType.Switch (MutationRate.fromFloat 0.02);
            poolCount=SorterCount.fromInt pc;
            rngGen=rg;
            fitnessFunc=FitnessFunc.standardSwitch 4.0;
        }
    RandoCollections.IndexedSeedGen rngGen |> Seq.map(fun (dex, rg) -> pm rg dex)


  let SplitPoolGenMut (poolSize:int) (dex:int) =
    match (dex % 6) with
    | 0 -> poolSize/2, 2, MutationType.Switch (MutationRate.fromFloat 0.01)
    | 1 -> poolSize/2, 2, MutationType.Switch (MutationRate.fromFloat 0.015)
    | 2 -> poolSize/2, 2, MutationType.Switch (MutationRate.fromFloat 0.02)
    | 3 -> poolSize/2, 2, MutationType.Stage (MutationRate.fromFloat 0.08)
    | 4 -> poolSize/2, 2, MutationType.Stage (MutationRate.fromFloat 0.12)
    | _ -> poolSize/2, 2, MutationType.Stage (MutationRate.fromFloat 0.16)


  let SplitPoolGenMut2 (poolSize:int) (dex:int) =
      match (dex % 3) with
      | 0 -> poolSize/16, 16, MutationType.Stage (MutationRate.fromFloat 0.12)
      | 1 -> poolSize/8, 8, MutationType.Stage (MutationRate.fromFloat 0.10)
      | _ -> poolSize/4, 4, MutationType.Stage (MutationRate.fromFloat 0.08)


  let ParamsM (rngGen:RngGen) (poolCount:int) =
      let pm rg dex =
          let gc, pc, mut = SplitPoolGenMut2 poolCount dex
          {
              generationNumber=GenerationNumber.fromInt gc;
              mutationType=mut;
              poolCount=SorterCount.fromInt pc;
              rngGen=rg;
              fitnessFunc=FitnessFunc.standardSwitch 4.0;
          }
      RandoCollections.IndexedSeedGen rngGen |> Seq.map(fun (dex, rg) -> pm rg dex)


type PoolUpdateParams = 
        {breederFrac:PoolFraction;
        generationNumber:GenerationNumber;
        mutationType:MutationType;
        poolCount:SorterCount;
        rngGen:RngGen;
        fitnessFunc:FitnessFunc;
        winnerFrac:PoolFraction;}


module PoolUpdateParams =
    
      let MutationTypes =
          let fr r = MutationRate.create "" r |> Result.ExtractOrThrow
          let fts r = MutationType.Stage (fr r)
          let ftw r = MutationType.Switch (fr r)
          let svs = seq {0.06 .. 0.02 .. 0.16} |> Seq.map(fts)
          let svw = seq {0.06 .. 0.02 .. 0.16} |> Seq.map(ftw)
          svs |> Seq.append svw |> Seq.toArray


      let SplitPoolGen (poolSize:int) (dex:int) =
        match (dex % 3) with
        | 0 -> poolSize/2, 2
        | 1 -> poolSize/4, 4
        | _ -> poolSize/8, 8
 

      let SplitPoolGenBnWFrac (poolSize:int) (dex:int) =
          match (dex % 6) with
          | 0 -> poolSize/2, 2, (PoolFraction.fromFloat 0.5), (PoolFraction.fromFloat 0.5)
          | 1 -> poolSize/4, 4, (PoolFraction.fromFloat 0.25), (PoolFraction.fromFloat 0.25)
          | 2 -> poolSize/8, 8, (PoolFraction.fromFloat 0.125), (PoolFraction.fromFloat 0.125)
          | 3 -> poolSize/2, 2, (PoolFraction.fromFloat 0.5), (PoolFraction.fromFloat 0.5)
          | 4 -> poolSize/4, 4, (PoolFraction.fromFloat 0.5), (PoolFraction.fromFloat 0.25)
          | _ -> poolSize/8, 8, (PoolFraction.fromFloat 0.5), (PoolFraction.fromFloat 0.125)


      let Params10 (rngGen:RngGen) (poolSize:int) =
        let pm rg dex =
            let gc, pc, bFrac, wFrac  = SplitPoolGenBnWFrac poolSize dex
            {
                breederFrac=bFrac;
                generationNumber=GenerationNumber.fromInt gc;
                mutationType=MutationType.Switch (MutationRate.fromFloat 0.02);
                poolCount=SorterCount.fromInt pc;
                rngGen=rg;
                fitnessFunc=FitnessFunc.standardSwitch 4.0;
                winnerFrac=wFrac;
            }
        RandoCollections.IndexedSeedGen rngGen |> Seq.map(fun (dex, rg) -> pm rg dex)


      let SplitPoolGenMut (poolGenCount:PoolGenCount) (dex:int) =
        match (dex % 6) with

        | 0 -> (PoolGenCount.value poolGenCount) / 2, 2, MutationType.Switch (MutationRate.fromFloat 0.01)
        | 1 -> (PoolGenCount.value poolGenCount) / 2, 2, MutationType.Switch (MutationRate.fromFloat 0.015)
        | 2 -> (PoolGenCount.value poolGenCount) / 2, 2, MutationType.Switch (MutationRate.fromFloat 0.02)
        | 3 -> (PoolGenCount.value poolGenCount) / 2, 2, MutationType.Stage (MutationRate.fromFloat 0.08)
        | 4 -> (PoolGenCount.value poolGenCount) / 2, 2, MutationType.Stage (MutationRate.fromFloat 0.12)
        | _ -> (PoolGenCount.value poolGenCount) / 2, 2, MutationType.Stage (MutationRate.fromFloat 0.16)


      let SplitPoolGenMut2 (poolGenCount:PoolGenCount) (dex:int) =
          match (dex % 3) with
          | 0 -> (PoolGenCount.value poolGenCount) / 16, 16, MutationType.Stage (MutationRate.fromFloat 0.12)
          | 1 -> (PoolGenCount.value poolGenCount) / 8, 8, MutationType.Stage (MutationRate.fromFloat 0.10)
          | _ -> (PoolGenCount.value poolGenCount) / 4, 4, MutationType.Stage (MutationRate.fromFloat 0.08)


      let ParamsM (rngGen:RngGen) (poolGenCount:PoolGenCount) =
          let pm rg dex =
              let gc, pc, mut = SplitPoolGenMut2 poolGenCount dex
              {
                  breederFrac=(PoolFraction.fromFloat 0.5);
                  generationNumber=GenerationNumber.fromInt gc;
                  mutationType=mut;
                  poolCount=SorterCount.fromInt pc;
                  rngGen=rg;
                  fitnessFunc=FitnessFunc.standardSwitch 4.0;
                  winnerFrac=(PoolFraction.fromFloat 0.5);
              }
          RandoCollections.IndexedSeedGen rngGen |> Seq.map(fun (dex, rg) -> pm rg dex)


type RndSorterParams = 
    {degree:Degree;
     sorterCount:SorterCount;
     rngGen:RngGen;
     sorterLength:SorterLength}


module RndSorterParams =

    let Make (degree:Degree) (sorterCount:SorterCount) 
             (rngGen:RngGen) (switchOrStage:string) =
    
        result {
            let! sorterLength = SorterLength.to999Sucessful degree switchOrStage
            return 
                {RndSorterParams.degree=degree;
                sorterCount=sorterCount;
                rngGen=rngGen;
                sorterLength = sorterLength}
        }


