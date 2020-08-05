namespace Archie.core.test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Archie.Base.SorterParts
open BenchmarkDotNet.Attributes
open Archie.Base.SortersFromData
open System

//|    Method |     Mean |    Error |   StdDev | Ratio | RatioSD |
//|---------- |---------:|---------:|---------:|------:|--------:|
//|   SortAll | 10.16 ms | 0.201 ms | 0.240 ms |  1.00 |    0.00 |
//|  SortAllT | 10.34 ms | 0.192 ms | 0.171 ms |  1.03 |    0.02 |
//| SortAllTR | 11.59 ms | 0.183 ms | 0.171 ms |  1.15 |    0.03 |
//| SortAllTB | 15.88 ms | 0.306 ms | 0.340 ms |  1.57 |    0.04 |


type SorterFullTestSlice() =
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorter16 = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
    let sortableSet = SortableSet.AllBinary degree |> Result.ExtractOrThrow

    [<Benchmark(Baseline = true)>]
    member this.SortAll() =
        SorterOps.SortAll sorter16 sortableSet.sortables
        SortableSet.Reset sortableSet

    [<Benchmark>]
    member this.SortAllT() =
        let res = SorterOps.SortAllT sorter16 sortableSet.sortables
        SortableSet.Reset sortableSet
        res

    [<Benchmark>]
    member this.SortAllTR() =
        let res = SorterOps.SortAllTR sorter16 sortableSet.sortables
        SortableSet.Reset sortableSet
        res

    [<Benchmark>]
    member this.SortAllTB() =
        let res = SorterOps.SortAllTB sorter16 sortableSet.sortables
        SortableSet.Reset sortableSet
        res


//|   Method |     Mean |    Error |   StdDev | Ratio | RatioSD |
//|--------- |---------:|---------:|---------:|------:|--------:|
//| SortSpTB |  2.849 s | 0.0174 s | 0.0163 s |  1.00 |    0.00 |
//|   SortTB |  3.267 s | 0.0125 s | 0.0104 s |  1.15 |    0.01 |
//| SortSpTR | 13.992 s | 0.0373 s | 0.0349 s |  4.91 |    0.03 |
//|   SortTR | 13.979 s | 0.0297 s | 0.0263 s |  4.90 |    0.03 |
type SorterSetFullTest() =
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let stageCount = (StageCount.create "" 200) |> Result.ExtractOrThrow
    let sorterCount = (SorterCount.create "" 100) |> Result.ExtractOrThrow
    let switchCount = (SwitchCount.create "" 1600) |> Result.ExtractOrThrow
    let seed = RandomSeed.create "" 4124 |> Result.ExtractOrThrow
    let randoLcg = new RandomLcg(seed) :> IRando

    let stagePackedSet = SorterSet.createRandomStagePacked degree stageCount
                            sorterCount randoLcg
    let sorterSet = SorterSet.createRandom degree switchCount  sorterCount randoLcg
    let sortableSet = SortableSet.AllBinary degree |> Result.ExtractOrThrow

    //[<Benchmark(Baseline = true)>]
    //member this.SortSpTB() =
    //    SortingRun.RunSorterSetOnSortableSetTB sortableSet stagePackedSet

    [<Benchmark>]
    member this.SortTB() =
        SortingRun.RunSorterSetOnSortableSetTB sortableSet sorterSet

    //[<Benchmark>]
    //member this.SortSpTR() =
    //    SortingRun.RunSorterSetOnSortableSetTR sortableSet stagePackedSet

    //[<Benchmark>]
    //member this.SortTR() =
    //    SortingRun.RunSorterSetOnSortableSetTR sortableSet sorterSet

    [<Benchmark(Baseline = true)>]
    member this.SortTBp() =
        SortingRun.RunSorterSetOnSortableSetTBp sortableSet sorterSet


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

        let sorterSet = SorterSet2.createRandomStagePacked degree stageCount
                                   sorterCount randoLcg randoLcg2

        let sortableSet = SortableSet.AllBinary degree |> Result.ExtractOrThrow

        let res = SortingRun.RunSorterSetOnSortableSetTB2 sortableSet sorterSet
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

        let sorterSet = SorterSet2.createRandomStagePacked degree stageCount
                                   sorterCount randoLcg randoLcg2

        let sortableSet = SortableSet.AllBinary degree |> Result.ExtractOrThrow

        let res = SortingRun.RunSorterSetOnSortableSetTB2p sortableSet sorterSet
        let goodies = res |> Array.map(fun r -> entroB r)
        goodies |> Array.iter(fun v-> Console.WriteLine(sprintf "%A  %f" (EntityId.value (Dependent.id v)) (Dependent.character v) ))
        //let goodies2 = res |> Array.map(fun r -> (fst r)|> SwitchTracker.UseTotal)
        //goodies2 |> Array.iter(fun v-> Console.WriteLine(sprintf "%A" v))




//| Method |      Mean |     Error |    StdDev | Ratio | RatioSD |
//|------- |----------:|----------:|----------:|------:|--------:|
//|  Short |  3.306 ms | 0.0496 ms | 0.0440 ms |  1.00 |    0.00 |
//|   Long | 31.854 ms | 0.5132 ms | 0.4285 ms |  9.65 |    0.15 |

type ArrayCopy() =

    let arroyoA = Array.create 6400000 5
    let arroyoB = Array.create 6400000 6

    let arroyoAw = Array.create 64000000 5
    let arroyoBw = Array.create 64000000 6

    member this.OA() =
        arroyoA
    member this.OB() =
        arroyoB

    member this.WA() =
        arroyoAw
    member this.WB() =
        arroyoBw

    [<Benchmark(Baseline = true)>]
    member this.Short() =
        Array.Copy(arroyoA, arroyoB, 6400000)

    [<Benchmark>]
    member this.Long() =
        Array.Copy(arroyoAw, arroyoBw, 64000000)




[<TestClass>]
type BenchmarkFixture () =

    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorter16 = RefSorter.CreateRefSorter RefSorter.End16 |> Result.ExtractOrThrow
    let sorterTrim = result {
                            let! fullLen = RefSorter.CreateRefSorter RefSorter.End16
                            let! trimLen = SwitchCount.create "" 59
                            return! Sorter.TrimLength fullLen trimLen
                        } |> Result.ExtractOrThrow

    let baseArray = IntBits.AllBinaryTestCasesArray (Degree.value degree)
                    |> Array.collect(fun a -> a)
    let backArray = Array.zeroCreate baseArray.Length
    let res = Array.Copy(baseArray, backArray, baseArray.Length)

    let SortableArray = Sortable.FromIntArray degree baseArray |> Seq.toArray

    //[<TestMethod>]
    //member this.Spano() =
    //    let res = new Spano()
    //    let rOA1 = res.OA()
    //    let rOB1 = res.OB()
    //    res.ShortItS()
    //    Assert.IsTrue (true)

    [<TestMethod>]
     member this.Flatten() =
         let degree = (Degree.create "" 16 ) |>Result.ExtractOrThrow
         let intCases = IntBits.AllBinaryTestCasesArray (Degree.value degree)
         let flatto = intCases |> Array.collect(fun a -> a)
         Assert.IsTrue (true)

    [<TestMethod>]
     member this.TCUP() =
         let res = new SorterFullTestSlice()
         let rS = res.SortAllTR()
         let rS1 = res.SortAllTR()
         let rS2 = res.SortAllTR()

         let useTrack, lastTrack = res.SortAllTB()
         Console.WriteLine (sprintf "useTrack: %A" useTrack)
         Console.WriteLine (sprintf "lastTrack: %A" lastTrack)
         Assert.IsTrue (true)

     [<TestMethod>]
     member this.TR() =
         let t, s = SorterOps.SortAllTR sorterTrim SortableArray
         let t2, s2 = SorterOps.SortAllTR sorterTrim SortableArray
         Array.Copy(backArray, baseArray, baseArray.Length)
         let t3, s3 = SorterOps.SortAllTR sorter16 SortableArray

         Assert.IsTrue (true)

