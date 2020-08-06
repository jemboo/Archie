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

type SorterFullTestSlice() =
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorter16 = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
    let sortableSet = SortableSet3.allBinary degree |> Result.ExtractOrThrow

    [<Benchmark(Baseline = true)>]
    member this.SortAll() =
        SorterOps3.SortAll sorter16 sortableSet

    [<Benchmark>]
    member this.SortAllT() =
        let res = SorterOps3.SortAllT sorter16 sortableSet
        res

    [<Benchmark>]
    member this.SortAllTR() =
        let res = SorterOps3.SortAllTR sorter16 sortableSet
        res

    [<Benchmark>]
    member this.SortAllTB() =
        let res = SorterOps3.SortAllTB sorter16 sortableSet
        res

    [<Benchmark>]
    member this.SortAllTBp() =
        let res = SorterOps3.SortAllTB sorter16 sortableSet
        res

    [<Benchmark>]
    member this.SortAllTBEp() =
        let res = SorterOps3.SortAllTB sorter16 sortableSet
        res


type SorterFullTestSlice2() =
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorter16 = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
    let sortableSet = SortableSet3.allBinary degree |> Result.ExtractOrThrow

    //[<Benchmark(Baseline = true)>]
    //member this.SortAll() =
    //    SorterOps2.SortAll sorter16 sortableSet

    //[<Benchmark>]
    //member this.SortAllT() =
    //    let res = SorterOps2.SortAllT sorter16 sortableSet
    //    res

    [<Benchmark>]
    member this.SortAllTR() =
        let res = SorterOps2.SortAllTR sorter16 sortableSet
        res

    //[<Benchmark>]
    //member this.SortAllTB() =
    //    let res = SorterOps2.SortAllTB sorter16 sortableSet
    //    res


//|    Method |     Mean |    Error |   StdDev | Ratio | RatioSD |
//|---------- |---------:|---------:|---------:|------:|--------:|
//|   SortAll | 13.16 ms | 0.263 ms | 0.693 ms |  1.00 |    0.00 |
//|  SortAllT | 13.39 ms | 0.285 ms | 0.841 ms |  1.02 |    0.08 |
//| SortAllTR | 14.68 ms | 0.291 ms | 0.827 ms |  1.12 |    0.09 |
//| SortAllTB | 19.25 ms | 0.394 ms | 1.162 ms |  1.46 |    0.11 |

type SorterFullTestSlice3() =
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorter16 = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
    let sortableSet = SortableSet3.allBinary degree |> Result.ExtractOrThrow

    [<Benchmark(Baseline = true)>]
    member this.SortAll() =
        SorterOps3.SortAll sorter16 sortableSet

    [<Benchmark>]
    member this.SortAllT() =
        let res = SorterOps3.SortAllT sorter16 sortableSet
        res

    [<Benchmark>]
    member this.SortAllTR() =
        let res = SorterOps3.SortAllTR sorter16 sortableSet
        res

    [<Benchmark>]
    member this.SortAllTB() =
        let res = SorterOps3.SortAllTB sorter16 sortableSet
        res



//|   Method |    Mean |    Error |   StdDev | Ratio | RatioSD |
//|--------- |--------:|---------:|---------:|------:|--------:|
//|   SortTB | 3.066 s | 0.0098 s | 0.0092 s |  1.00 |    0.00 |
//|  SortTBp | 1.131 s | 0.0430 s | 0.1269 s |  0.30 |    0.02 |
//|  SortTB2 | 3.104 s | 0.0162 s | 0.0152 s |  1.01 |    0.01 |
//| SortTB2p | 1.156 s | 0.0454 s | 0.1338 s |  0.31 |    0.02 |

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

    let sortableSet = SortableSet3.allBinary degree |> Result.ExtractOrThrow

    let sorter16s = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
                   |> Seq.replicate 100 
    let sorterSetS = SorterSet.fromSorters degree sorter16s

    let sorterSetSE = SorterSetE.fromSorters degree sorter16s randoLcg randoLcg2

    //[<Benchmark(Baseline = true)>]
    //member this.SortTB() =
    //    SortingRun.RunSorterSetOnSortableSetTB sortableSet sorterSet

    //[<Benchmark>]
    //member this.SortTBp() =
    //    SortingRun.RunSorterSetOnSortableSetTBp sortableSet sorterSet

    //[<Benchmark>]
    //member this.SortTB2() =
    //    SortingRun.RunSorterSetOnSortableSetTB2 sortableSet sorterSetE

    //[<Benchmark>]
    //member this.SortTB2p() =
    //    SortingRun.RunSorterSetOnSortableSetTB2p sortableSet sorterSetE

    [<Benchmark>]
    member this.SortTBpC() =
        SortingRun.RunSorterSetOnSortableSetTBp sortableSet sorterSetS

    [<Benchmark>]
    member this.SortTB2pC() =
        SortingRun.RunSorterSetOnSortableSetTB2p sortableSet sorterSetSE



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

        let sortableSet = SortableSet3.allBinary degree |> Result.ExtractOrThrow

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

        let sorterSet = SorterSetE.createRandomStagePacked degree stageCount
                                   sorterCount randoLcg randoLcg2

        let sortableSet = SortableSet3.allBinary degree |> Result.ExtractOrThrow

        let res = SortingRun.RunSorterSetOnSortableSetTB2p sortableSet sorterSet
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

        let sortableSet = SortableSet3.allBinary degree |> Result.ExtractOrThrow

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


    [<TestMethod>]
    member this.Spano() =

        Assert.IsTrue (true)

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
    member this.TR2() =
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let sorter16 = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
        let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow
        SorterOps2.SortAll sorter16 sortableSet
        SorterOps2.SortAll sorter16 sortableSet
        Assert.IsTrue (true)

