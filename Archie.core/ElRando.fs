
namespace Archie.Base
open System


type RandomNet(seed:RandomSeed) =
    let mutable _count = 0
    let rnd = new System.Random(RandomSeed.value seed)

    interface IRando with
        member this.Seed = seed
        member this.Count = _count
        member this.NextUInt =
            _count <- _count + 2
            let vv = (uint32 (rnd.Next()))
            vv + (uint32 (rnd.Next()))
        member this.NextPositiveInt =
            rnd.Next()
        member this.NextULong =
            let r = this :> IRando
            let vv = (uint64 r.NextUInt)
            (vv <<< 32) + (uint64 r.NextUInt)

        member this.NextFloat = 
            _count <- _count + 1
            rnd.NextDouble()

        member this.RngType = RngType.Net


type RandomLcg(seed:RandomSeed) =
    let _a=6364136223846793005UL
    let _c=1442695040888963407UL
    let mutable _last = (_a * (uint64 (RandomSeed.value seed)) + _c)
    let mutable _count = 0
    member this.Seed = seed
    member this.Count = _count
    member this.NextUInt =
        _count <- _count + 1
        _last <- ((_a * _last) + _c)
        (uint32 (_last >>> 32))

    member this.NextULong =
        let mm = ((_a * _last) + _c)
        _last <- ((_a * mm) + _c)
        _count <- _count + 2
        _last + (mm >>> 32)

    member this.NextFloat = 
        (float this.NextUInt) / (float Microsoft.FSharp.Core.uint32.MaxValue)

    interface IRando with
        member this.Seed = this.Seed
        member this.Count = _count
        member this.NextUInt =
            this.NextUInt
        member this.NextPositiveInt =
            int (this.NextUInt >>> 1)
        member this.NextULong =
            this.NextULong
        member this.NextFloat = 
            this.NextFloat
        member this.RngType = RngType.Lcg

     
module Rando =
    
    let NetFromSeed seed =
        let seed = RandomSeed.create "" seed |> Result.ExtractOrThrow
        new RandomNet(seed) :> IRando


    let LcgFromSeed seed =
        let seed = RandomSeed.create "" seed |> Result.ExtractOrThrow
        new RandomLcg(seed) :> IRando


    let private _NextGuid (curr:IRando) : System.Guid =
                GuidUtils.makeGuid (curr.NextULong) (curr.NextULong) 
                                   (curr.NextULong) (curr.NextULong)


    let private _NextGuid2 (curr1:IRando) (curr2:IRando) : System.Guid =
                GuidUtils.makeGuid (curr1.NextULong) (curr1.NextULong) 
                                   (curr2.NextULong) (curr2.NextULong)


    let NextGuid (curr1:IRando) (curr2:IRando option) : System.Guid =
        match curr2 with
        | Some rando -> _NextGuid2 curr1 rando
        | None -> _NextGuid curr1


    let fromSeed rngtype seed =
        match rngtype with
        | RngType.Lcg -> LcgFromSeed seed
        | RngType.Net -> NetFromSeed seed


    let fromGuid rngType (gu:Guid) =
        fromSeed rngType (gu.GetHashCode())


    let getSeed = 
        (RandomSeed.create "" (int System.DateTime.Now.Ticks))


    let fromRngGen (rngGen:RngGen) =
        match rngGen.rngType with
        | RngType.Lcg -> LcgFromSeed (RandomSeed.value rngGen.seed)
        | RngType.Net -> NetFromSeed (RandomSeed.value rngGen.seed)


    let multiDraw (rnd:IRando) (freq:float) (numDraws:int)  =
        let draw (randy:IRando) =
            if randy.NextFloat < freq then 1 else 0
        let mutable i=0
        let mutable successCount = 0
        while (i < numDraws) do
                successCount <- successCount + draw rnd
                i <- i + 1
        successCount


module RandoCollections =

    let IndexedRandomData (rngGen:RngGen) (f:IRando->'a) = 
      let rando = Rando.fromSeed rngGen.rngType (RandomSeed.value rngGen.seed)
      Seq.initInfinite(fun i ->  (i, (f rando)) )


    let IndexedRandomData2 (rngGen:RngGen) (rngGen2:RngGen option) 
                           (f:IRando->IRando option->'a) = 
        let rando = Rando.fromSeed rngGen.rngType (RandomSeed.value rngGen.seed)
        match rngGen2 with
        | Some rg -> let rando2 = Rando.fromSeed rngGen.rngType (RandomSeed.value rngGen.seed)
                     Seq.initInfinite(fun i ->  (i, (f rando (Some rando2))) )
        | None -> Seq.initInfinite(fun i ->  (i, (f rando None)) )


    let IndexedSeedGen (rngGen:RngGen) =
        IndexedRandomData
          rngGen
          (fun rando -> {
              RngGen.rngType=rngGen.rngType; 
              RngGen.seed = RandomSeed.fromInt rando.NextPositiveInt })


    let IndexedGuidGen2 (rngGen:RngGen) (rngGen2:RngGen option) = 
        IndexedRandomData2 
            rngGen rngGen2
            (fun rando rando2 -> Rando.NextGuid rando rando2)

    let IndexedGuidGen (rngGen:RngGen) = 
        IndexedRandomData
            rngGen
            (fun rando -> Rando.NextGuid rando None)


