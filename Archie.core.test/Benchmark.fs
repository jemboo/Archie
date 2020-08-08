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


//|  Method |       Mean |    Error |   StdDev | Ratio |
//|-------- |-----------:|---------:|---------:|------:|
//|  SortTR | 1,549.1 ms | 30.91 ms | 67.20 ms |  1.00 |
//| SortTRp |   238.6 ms |  4.72 ms | 10.94 ms |  0.15 |

//|   Method |     Mean |   Error |   StdDev | Ratio | RatioSD |
//|--------- |---------:|--------:|---------:|------:|--------:|
//|  SortTBp | 294.7 ms | 5.83 ms | 13.28 ms |  1.00 |    0.00 |
//| SortTBEp | 299.0 ms | 5.79 ms |  8.66 ms |  1.01 |    0.05 |

type SorterSetRandomTest() =
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorterCount = (SorterCount.create "" 24) |> Result.ExtractOrThrow
    let switchCount = (SwitchCount.create "" 1600) |> Result.ExtractOrThrow
    let seed = RandomSeed.create "" 41324 |> Result.ExtractOrThrow
    let seed2 = RandomSeed.create "" 4124 |> Result.ExtractOrThrow
    let randoLcg = new RandomLcg(seed) :> IRando
    let randoLcg2 = new RandomLcg(seed2) :> IRando

    let sorterSetRnd = SorterSet.createRandom degree switchCount sorterCount randoLcg
    let sorterSetRndE = SorterSetE.fromSorterSet sorterSetRnd randoLcg randoLcg2

    let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow

    let sorter16s = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
                   |> Seq.replicate (SorterCount.value sorterCount) 
    let sorterSet16 = SorterSet.fromSorters degree sorter16s
    let sorterSet16E = SorterSetE.fromSorterSet sorterSet16 randoLcg randoLcg2

    [<Benchmark(Baseline = true)>]
    member this.Sort16TRp() =
        SortingRun.RunSorterSetOnSortableSetTR sortableSet sorterSet16 true

    [<Benchmark>]
    member this.Sort16TREp() =
        SortingRun.RunSorterSetOnSortableSetTRE sortableSet sorterSet16E true

    [<Benchmark>]
    member this.Sort16TR() =
        SortingRun.RunSorterSetOnSortableSetTR sortableSet sorterSet16 false

    [<Benchmark>]
    member this.Sort16TBEp() =
        SortingRun.RunSorterSetOnSortableSetTBE sortableSet sorterSet16E true

    [<Benchmark>]
    member this.SortRandTREp() =
        SortingRun.RunSorterSetOnSortableSetTRE sortableSet sorterSetRndE true

    [<Benchmark>]
    member this.SortRandTBEp() =
        SortingRun.RunSorterSetOnSortableSetTBE sortableSet sorterSetRndE true

    [<Benchmark>]
    member this.SortTBE() =
        SortingRun.RunSorterSetOnSortableSetTB sortableSet sorterSetRnd false



//|               Method |         Mean |       Error |      StdDev |  Ratio | RatioSD |
//|--------------------- |-------------:|------------:|------------:|-------:|--------:|
//|            CopyArray |     426.6 us |     3.46 us |     3.07 us |   1.00 |    0.00 |
//|          CopyArrayZc | 103,357.0 us | 2,060.55 us | 2,204.77 us | 241.21 |    5.84 |
//| CreateAndCopyArrayZc | 102,811.5 us | 2,046.12 us | 3,245.36 us | 241.43 |    8.21 |
//|   CreateAndCopyArray |   5,405.9 us |   174.55 us |   514.67 us |  12.33 |    1.05 |
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

