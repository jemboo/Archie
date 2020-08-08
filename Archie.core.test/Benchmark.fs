namespace Archie.core.test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Archie.Base.SorterParts
open BenchmarkDotNet.Attributes
open Archie.Base.SortersFromData
open System


//|---------- |---------:|---------:|---------:|------:|--------:|
//|   SortAll | 13.90 ms | 0.290 ms | 0.855 ms |  1.00 |    0.00 |
//|  SortAllT | 14.38 ms | 0.288 ms | 0.839 ms |  1.04 |    0.09 |
//| SortAllTR | 14.71 ms | 0.292 ms | 0.860 ms |  1.06 |    0.09 |
//| SortAllTB | 20.50 ms | 0.416 ms | 1.225 ms |  1.48 |    0.13 |

type BenchmarkSorterOps() =
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorter16 = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
    let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow

    [<Benchmark>]
    member this.SortAllTR() =
        let res = SorterOps.SortAllTR sorter16 sortableSet
        res

    [<Benchmark>]
    member this.SortAllTB() =
        let res = SorterOps.SortAllTB sorter16 sortableSet
        res




//|   Method |    Mean |    Error |   StdDev | Ratio | RatioSD |
//|--------- |--------:|---------:|---------:|------:|--------:|
//|   SortTB | 3.066 s | 0.0098 s | 0.0092 s |  1.00 |    0.00 |
//|  SortTBp | 1.131 s | 0.0430 s | 0.1269 s |  0.30 |    0.02 |
//|  SortTB2 | 3.104 s | 0.0162 s | 0.0152 s |  1.01 |    0.01 |
//| SortTB2p | 1.156 s | 0.0454 s | 0.1338 s |  0.31 |    0.02 |

//|   Method |       Mean |    Error |   StdDev | Ratio | RatioSD |
//|--------- |-----------:|---------:|---------:|------:|--------:|
//|   SortTB | 3,753.4 ms | 72.73 ms | 77.82 ms |  1.00 |    0.00 |
//|  SortTBp |   585.0 ms | 10.70 ms | 19.03 ms |  0.15 |    0.01 |
//|  SortTB2 | 3,371.9 ms | 50.46 ms | 44.73 ms |  0.90 |    0.03 |
//| SortTB2p |   560.6 ms | 10.94 ms | 19.72 ms |  0.15 |    0.01 |


type SorterSetRandomTest() =
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let stageCount = (StageCount.create "" 200) |> Result.ExtractOrThrow
    let sorterCount = (SorterCount.create "" 100) |> Result.ExtractOrThrow
    let switchCount = (SwitchCount.create "" 1600) |> Result.ExtractOrThrow
    let seed = RandomSeed.create "" 41324 |> Result.ExtractOrThrow
    let seed2 = RandomSeed.create "" 4124 |> Result.ExtractOrThrow
    let randoLcg = new RandomLcg(seed) :> IRando
    let randoLcg2 = new RandomLcg(seed2) :> IRando

    let stagePackedSet = SorterSet.createRandomStagePacked degree stageCount
                            sorterCount randoLcg
    let sorterSet = SorterSet.createRandom degree switchCount sorterCount randoLcg
    let sorterSetE = SorterSetE.createRandom degree switchCount sorterCount randoLcg randoLcg2

    let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow

    let sorter16s = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
                   |> Seq.replicate 100 
    let sorterSetS = SorterSet.fromSorters degree sorter16s

    let sorterSetSE = SorterSetE.fromSorters degree sorter16s randoLcg randoLcg2

    [<Benchmark(Baseline = true)>]
    member this.SortTR() =
        SortingRun.RunSorterSetOnSortableSetTR sortableSet sorterSet

    [<Benchmark>]
    member this.SortTRp() =
        SortingRun.RunSorterSetOnSortableSetTRp sortableSet sorterSet


    //[<Benchmark(Baseline = true)>]
    //member this.SortTB() =
    //    SortingRun.RunSorterSetOnSortableSetTB sortableSet sorterSet

    //[<Benchmark>]
    //member this.SortTBp() =
    //    SortingRun.RunSorterSetOnSortableSetTBp sortableSet sorterSet

    //[<Benchmark>]
    //member this.SortTB2() =
    //    SortingRun.RunSorterSetOnSortableSetTBE sortableSet sorterSetE

    //[<Benchmark>]
    //member this.SortTB2p() =
    //    SortingRun.RunSorterSetOnSortableSetTBEp sortableSet sorterSetE

    //[<Benchmark>]
    //member this.SortTBpC() =
    //    SortingRun.RunSorterSetOnSortableSetTBp sortableSet sorterSetS

    //[<Benchmark>]
    //member this.SortTB2pC() =
    //    SortingRun.RunSorterSetOnSortableSetTBEp sortableSet sorterSetSE



type w2() =

    member this.ww() =
        let entroB (dtsp:Dependent<SwitchTracker*SwitchTracker>) =
            let rr = (Dependent.character dtsp) |> fst |> SwitchTracker.EntropyBits
            (Dependent.createD dtsp rr)

        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let switchCount = (SwitchCount.create "" 1600) |> Result.ExtractOrThrow
        let stageCount = (StageCount.create "" 200) |> Result.ExtractOrThrow
        let sorterCount = (SorterCount.create "" 100) |> Result.ExtractOrThrow
        let seed = RandomSeed.create "" 41324 |> Result.ExtractOrThrow
        let seed2 = RandomSeed.create "" 4124 |> Result.ExtractOrThrow
        let randoLcg = new RandomLcg(seed) :> IRando
        let randoLcg2 = new RandomLcg(seed2) :> IRando

        let sorterSet = SorterSetE.createRandomStagePacked degree stageCount
                                   sorterCount randoLcg randoLcg2

        let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow

        let res = SortingRun.RunSorterSetOnSortableSetTBE sortableSet sorterSet
        let goodies = res |> Array.map(fun r -> entroB r)
        goodies |> Array.iter(fun v-> Console.WriteLine(sprintf "%A  %f" (EntityId.value (Dependent.id v)) (Dependent.character v) ))
        //let goodies2 = res |> Array.map(fun r -> (fst r)|> SwitchTracker.UseTotal)
        //goodies2 |> Array.iter(fun v-> Console.WriteLine(sprintf "%A" v))


    member this.wwP() =
        let entroB (dtsp:Dependent<SwitchTracker*SwitchTracker>) =
            let rr = (Dependent.character dtsp) |> fst |> SwitchTracker.EntropyBits
            (Dependent.createD dtsp rr)

        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let switchCount = (SwitchCount.create "" 1600) |> Result.ExtractOrThrow
        let stageCount = (StageCount.create "" 200) |> Result.ExtractOrThrow
        let sorterCount = (SorterCount.create "" 100) |> Result.ExtractOrThrow
        let seed = RandomSeed.create "" 41324 |> Result.ExtractOrThrow
        let seed2 = RandomSeed.create "" 4124 |> Result.ExtractOrThrow
        let randoLcg = new RandomLcg(seed) :> IRando
        let randoLcg2 = new RandomLcg(seed2) :> IRando

        let sorterSet = SorterSetE.createRandomStagePacked degree stageCount
                                   sorterCount randoLcg randoLcg2

        let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow

        let res = SortingRun.RunSorterSetOnSortableSetTBEp sortableSet sorterSet
        let goodies = res |> Array.map(fun r -> entroB r)
        goodies |> Array.iter(fun v-> Console.WriteLine(sprintf "%A  %f" (EntityId.value (Dependent.id v)) (Dependent.character v) ))
        //let goodies2 = res |> Array.map(fun r -> (fst r)|> SwitchTracker.UseTotal)
        //goodies2 |> Array.iter(fun v-> Console.WriteLine(sprintf "%A" v))


    member this.wwP3() =
        let entroB (dtsp:Dependent<SwitchTracker*SwitchTracker>) =
            let rr = (Dependent.character dtsp) |> fst |> SwitchTracker.EntropyBits
            (Dependent.createD dtsp rr)

        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let switchCount = (SwitchCount.create "" 1600) |> Result.ExtractOrThrow
        let stageCount = (StageCount.create "" 200) |> Result.ExtractOrThrow
        let sorterCount = (SorterCount.create "" 100) |> Result.ExtractOrThrow
        let seed = RandomSeed.create "" 41324 |> Result.ExtractOrThrow
        let seed2 = RandomSeed.create "" 4124 |> Result.ExtractOrThrow
        let randoLcg = new RandomLcg(seed) :> IRando
        let randoLcg2 = new RandomLcg(seed2) :> IRando

        let sorterSet = SorterSetE.createRandomStagePacked degree stageCount
                                   sorterCount randoLcg randoLcg2

        let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow

        let res = SortingRun.RunSorterSetOnSortableSetTBEp sortableSet sorterSet
        let goodies = res |> Array.map(fun r -> entroB r)
        goodies |> Array.iter(fun v-> Console.WriteLine(sprintf "%A  %f" (EntityId.value (Dependent.id v)) (Dependent.character v) ))
        //let goodies2 = res |> Array.map(fun r -> (fst r)|> SwitchTracker.UseTotal)
        //goodies2 |> Array.iter(fun v-> Console.WriteLine(sprintf "%A" v))




//| Method |      Mean |     Error |    StdDev | Ratio | RatioSD |
//|------- |----------:|----------:|----------:|------:|--------:|
//|  Short |  3.306 ms | 0.0496 ms | 0.0440 ms |  1.00 |    0.00 |
//|   Long | 31.854 ms | 0.5132 ms | 0.4285 ms |  9.65 |    0.15 |
type ArrayCopy() =
    let sl = (1 <<< 20)
    let longLen = (1 <<< 25)
    let shortA = Array.create sl 5
    let shortB = Array.create sl 6

    let longA = Array.create longLen 5
    let longB = Array.create longLen 6
    //let arroyoA = Array.create 6400000 5
    //let arroyoB = Array.create 6400000 6

    //let arroyoAw = Array.create 64000000 5
    //let arroyoBw = Array.create 64000000 6

    //member this.ShortA() =
    //    shortA
    //member this.ShortB() =
    //    shortB

    //member this.LongA() =
    //    longA
    //member this.LongB() =
    //    longB

    [<Benchmark(Baseline = true)>]
    member this.Short() =
        Array.Copy(shortA, shortB, sl) |> ignore
        true

    [<Benchmark>]
    member this.Long() =
        Array.Copy(longA, longB, longLen) |> ignore
        true

//|      Method |         Mean |       Error |      StdDev | Ratio |
//|------------ |-------------:|------------:|------------:|------:|
//| CopyArrayZc | 103,051.1 us | 1,869.29 us | 1,748.54 us | 1.000 |
//|   CopyArray |     427.8 us |     3.83 us |     3.58 us | 0.004 |
type ZeroCreateTest() =
    let arrayLen = (1 <<< 20)
    let arrayZcA = Array.zeroCreate arrayLen
    let arrayZcB = Array.create arrayLen 123

    let arrayA = Array.create arrayLen 0
    let arrayB = Array.create arrayLen 123

    [<Benchmark(Baseline = true)>]
    member this.CopyArray() =
        Array.Copy(arrayB, arrayA, arrayLen)

    [<Benchmark>]
    member this.CopyArrayZc() =
        Array.Copy(arrayZcB, arrayZcA, arrayLen)

    [<Benchmark>]
    member this.CreateAndCopyArrayZc() =
        let aZcA = Array.zeroCreate arrayLen
        let aZcB = Array.create arrayLen 123
        Array.Copy(aZcB, aZcA, arrayLen)

    [<Benchmark>]
    member this.CreateAndCopyArray() =
        let aA = Array.create arrayLen 0
        let aB = Array.create arrayLen 123
        Array.Copy(aB, aA, arrayLen)


[<TestClass>]
type BenchmarkFixture () =

    [<TestMethod>]
    member this.ArrayCopy() =
        Assert.IsTrue (true)

