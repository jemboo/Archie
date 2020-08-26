﻿
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
        let pc0 = System.BitConverter.GetBytes(curr.NextULong)
        let pc1 = System.BitConverter.GetBytes(curr.NextULong)
        let pc2 = System.BitConverter.GetBytes(curr.NextULong)
        let pc3 = System.BitConverter.GetBytes(curr.NextULong)

        let woof = seq {pc0.[0]; pc0.[1]; pc0.[2]; pc0.[3]; 
                        pc1.[0]; pc1.[1]; pc1.[2]; pc1.[3];
                        pc2.[0]; pc2.[1]; pc2.[2]; pc2.[3];
                        pc3.[0]; pc3.[1]; pc3.[2]; pc3.[3]; } |> Seq.toArray
        new System.Guid(woof)

    let private _NextGuid2 (curr1:IRando) (curr2:IRando) : System.Guid =
        let pc0 = System.BitConverter.GetBytes(curr1.NextULong)
        let pc1 = System.BitConverter.GetBytes(curr1.NextULong)
        let pc2 = System.BitConverter.GetBytes(curr2.NextULong)
        let pc3 = System.BitConverter.GetBytes(curr2.NextULong)

        let woof = seq {pc0.[0]; pc0.[1]; pc0.[2]; pc0.[3]; 
                        pc1.[0]; pc1.[1]; pc1.[2]; pc1.[3];
                        pc2.[0]; pc2.[1]; pc2.[2]; pc2.[3];
                        pc3.[0]; pc3.[1]; pc3.[2]; pc3.[3]; } |> Seq.toArray
        new System.Guid(woof)


    let NextGuid (curr1:IRando) (curr2:IRando option) : System.Guid =
        match curr2 with
        | Some rando -> _NextGuid2 curr1 rando
        | None -> _NextGuid curr1


    let RandoFromSeed rngtype seed =
        match rngtype with
        | RngType.Lcg -> LcgFromSeed seed
        | RngType.Net -> NetFromSeed seed

    let GetSeed = 
        (RandomSeed.create "" (int System.DateTime.Now.Ticks))


module RngGenF =

    let createLcg (seed:int) =
        let rnd = (RandomSeed.create "" seed) |> Result.ExtractOrThrow
        {rngType=RngType.Lcg; seed=rnd}

    let createNet (seed:int) =
        let rnd = (RandomSeed.create "" seed) |> Result.ExtractOrThrow
        {rngType=RngType.Net; seed=rnd}

    let randoFromRngGen (rngGen:RngGen) =
        match rngGen.rngType with
        | RngType.Lcg -> Rando.LcgFromSeed (RandomSeed.value rngGen.seed)
        | RngType.Net -> Rando.NetFromSeed (RandomSeed.value rngGen.seed)
        
    let toDto (rngt:RngGen) =
        match rngt.rngType with
        | Lcg -> sprintf "Lcg %d" (RandomSeed.value rngt.seed)
        | Net -> sprintf "Net %d" (RandomSeed.value rngt.seed)

    let fromDto (str:string) =
        let finishParse (str:string) =
            match str with
            | "Lcg" -> RngType.Lcg |> Ok
            | "Net" -> RngType.Net |> Ok
            | _ -> Error (sprintf "no match for RngType: %s" str)
        let doArgs (pcs:string[]) =
            if pcs.Length = 2 then
                result {
                          let! seed = (ParseUtils.MakeInt32 pcs.[1])
                          let! rndSeed = RandomSeed.create "" seed
                          let! rng = finishParse pcs.[0]
                          return {RngGen.rngType=rng; RngGen.seed=rndSeed;}
                       }
              else
                  Error (sprintf "incorrect string: %s" str)
        result {
            let pcs = str.Split(" ", StringSplitOptions.RemoveEmptyEntries)
            return! doArgs pcs
        }
    