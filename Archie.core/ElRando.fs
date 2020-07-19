
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
        member this.NextULong =
            this.NextULong
        member this.NextFloat = 
            this.NextFloat
        member this.RngType = RngType.Lcg

        
module Rando = 
    let NextGuid (curr:IRando) : System.Guid =
        let pc0 = System.BitConverter.GetBytes(curr.NextULong)
        let pc1 = System.BitConverter.GetBytes(curr.NextULong)
        let pc2 = System.BitConverter.GetBytes(curr.NextULong)
        let pc3 = System.BitConverter.GetBytes(curr.NextULong)

        let woof = seq {pc0.[0]; pc0.[1]; pc0.[2]; pc0.[3]; 
                        pc1.[0]; pc1.[1]; pc1.[2]; pc1.[3];
                        pc2.[0]; pc2.[1]; pc2.[2]; pc2.[3];
                        pc3.[0]; pc3.[1]; pc3.[2]; pc3.[3]; } |> Seq.toArray
        new System.Guid(woof)

    let NextGuid2 (curr1:IRando) (curr2:IRando) : System.Guid =
        let pc0 = System.BitConverter.GetBytes(curr1.NextULong)
        let pc1 = System.BitConverter.GetBytes(curr1.NextULong)
        let pc2 = System.BitConverter.GetBytes(curr2.NextULong)
        let pc3 = System.BitConverter.GetBytes(curr2.NextULong)

        let woof = seq {pc0.[0]; pc0.[1]; pc0.[2]; pc0.[3]; 
                        pc1.[0]; pc1.[1]; pc1.[2]; pc1.[3];
                        pc2.[0]; pc2.[1]; pc2.[2]; pc2.[3];
                        pc3.[0]; pc3.[1]; pc3.[2]; pc3.[3]; } |> Seq.toArray
        new System.Guid(woof)