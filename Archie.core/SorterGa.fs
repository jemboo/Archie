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


type PoolUpdateParams = 
        {breederFrac:PoolFraction;
        generationCount:GenerationCount;
        mutationType:MutationType;
        poolCount:SorterCount;
        rngGen:RngGen;
        fitnessFunc:FitnessFunc;
        winnerFrac:PoolFraction;}


module PoolUpdateParams =
    
      let IndexedRandomData (rngGen:RngGen) (f:IRando->'a) = 
        let rando = Rando.RandoFromSeed rngGen.rngType (RandomSeed.value rngGen.seed)
        Seq.initInfinite(fun i ->  (i, (f rando)) )

      
      let IndexedRandomData2 (rngGen:RngGen) (rngGen2:RngGen option) 
                             (f:IRando->IRando option->'a) = 
          let rando = Rando.RandoFromSeed rngGen.rngType (RandomSeed.value rngGen.seed)
          match rngGen2 with
          | Some rg -> let rando2 = Rando.RandoFromSeed rngGen.rngType (RandomSeed.value rngGen.seed)
                       Seq.initInfinite(fun i ->  (i, (f rando (Some rando2))) )
          | None -> Seq.initInfinite(fun i ->  (i, (f rando None)) )


      let IndexedSeedGen (rngGen:RngGen) = 
          IndexedRandomData 
            rngGen 
            (fun rando -> {
                RngGen.rngType=rngGen.rngType; 
                RngGen.seed = RandomSeed.create "" rando.NextPositiveInt |> Result.ExtractOrThrow})


      let IndexedGuidGen (rngGen:RngGen) (rngGen2:RngGen option) = 
          IndexedRandomData2 
              rngGen rngGen2
              (fun rando rando2 -> Rando.NextGuid rando rando2)


      let Repeater f (items:'a[]) (count:int) =
          let tt = seq {for i=0 to (items.Length-1) do yield! Seq.replicate count (f items.[i]) }
          seq { while true do yield! tt }


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
                generationCount=GenerationCount.fromInt gc;
                mutationType=MutationType.Switch (MutationRate.fromFloat 0.02);
                poolCount=SorterCount.fromInt pc;
                rngGen=rg;
                fitnessFunc=FitnessFunc.standardSwitch 4.0;
                winnerFrac=wFrac;
            }
        IndexedSeedGen rngGen |> Seq.map(fun (dex, rg) -> pm rg dex)

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
                  breederFrac=(PoolFraction.fromFloat 0.5);
                  generationCount=GenerationCount.fromInt gc;
                  mutationType=mut;
                  poolCount=SorterCount.fromInt pc;
                  rngGen=rg;
                  fitnessFunc=FitnessFunc.standardSwitch 4.0;
                  winnerFrac=(PoolFraction.fromFloat 0.5);
              }
          IndexedSeedGen rngGen |> Seq.map(fun (dex, rg) -> pm rg dex)


type RndSorterPoolParams = 
    {degree:Degree;
     sorterCount:SorterCount;
     rngGen:RngGen;
     rndSorterGen:RndSorterGen}

module RndSorterPoolParams =
    let degreeToSwitchCount (degree:Degree) =
        let d = (Degree.value degree)
        let ct = match d with
                 | 10 -> 800
                 | 12 -> 1000
                 | 14 -> 1200
                 | 16 -> 1600
                 | 18 -> 2000
                 | 20 -> 2200
                 | 22 -> 2600
                 | 24 -> 3000
                 | _ -> 0
        let wc = SwitchCount.create "" ct |> Result.ExtractOrThrow
        RndSorterGen.Switch wc

    let degreeToStageCount (degree:Degree) =
        let d = (Degree.value degree)
        let ct = match d with
                | 10 -> 160
                | 12 -> 160
                | 14 -> 160
                | 16 -> 200
                | 18 -> 200
                | 20 -> 200
                | 22 -> 220
                | 24 -> 220
                | _ -> 0
        let tc = StageCount.create "" ct |> Result.ExtractOrThrow
        RndSorterGen.Stage tc

    let Make (degree:Degree) (sorterCount:SorterCount) 
             (rngGen:RngGen) (switchOrStage:string) =
        match switchOrStage with
        | "Switch" -> Some {degree=degree;
                            sorterCount=sorterCount;
                            rngGen=rngGen;
                            rndSorterGen=(degreeToSwitchCount degree)}
        | "Stage" -> Some {degree=degree;
                           sorterCount=sorterCount;
                           rngGen=rngGen;
                           rndSorterGen=(degreeToStageCount degree)}
        | _ -> None

