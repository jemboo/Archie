namespace Archie.Benchmark
open Archie.Base
open BenchmarkDotNet.Attributes
open System.Security.Cryptography
open Archie.Base.SortersFromData
open System


//|    Method |     Mean |    Error |   StdDev |
//|---------- |---------:|---------:|---------:|
//| SortAllTR | 16.09 ms | 0.320 ms | 0.820 ms |
//| SortAllTB | 20.40 ms | 0.436 ms | 1.286 ms |
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


//|        Method |       Mean |    Error |   StdDev | Ratio |
//|-------------- |-----------:|---------:|---------:|------:|
//|  CompleteSort | 3,996.8 ms | 26.94 ms | 25.20 ms |  1.00 |
//|  StopIfSorted |   857.7 ms | 17.02 ms | 21.53 ms |  0.21 |
//| CompleteSortP |   663.8 ms |  6.60 ms |  5.51 ms |  0.17 |
//| StopIfSortedP |   148.9 ms |  2.88 ms |  4.22 ms |  0.04 |
type SorterSetRandomTest() =
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorterCount = (SorterCount.create "" 24) |> Result.ExtractOrThrow
    let switchCount = (SwitchCount.create "" 1600) |> Result.ExtractOrThrow
    let randSorterGen = SorterLength.Switch switchCount
    let seed = RandomSeed.create "" 41324 |> Result.ExtractOrThrow
    let randoLcg = new RandomLcg(seed) :> IRando

    let sorterSetRnd = SorterSet.createRandom degree randSorterGen None sorterCount randoLcg
    let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow

    [<Benchmark(Baseline = true)>]
    member this.CompleteSort() =
        SorterOps.CompleteSort sortableSet sorterSetRnd.sorters false

    [<Benchmark>]
    member this.StopIfSorted() =
        SorterOps.StopIfSorted sortableSet sorterSetRnd.sorters false

    [<Benchmark>]
    member this.CompleteSortP() =
        SorterOps.CompleteSort sortableSet sorterSetRnd.sorters true

    [<Benchmark>]
    member this.StopIfSortedP() =
        SorterOps.StopIfSorted sortableSet sorterSetRnd.sorters true


////|        Method |      Mean |     Error |    StdDev | Ratio | RatioSD |
////|-------------- |----------:|----------:|----------:|------:|--------:|
////|  CompleteSort | 361.19 ms | 11.272 ms | 33.235 ms |  1.00 |    0.00 |
////|  StopIfSorted | 490.56 ms | 10.566 ms | 31.155 ms |  1.37 |    0.17 |
////| CompleteSortP |  58.75 ms |  1.171 ms |  2.619 ms |  0.17 |    0.02 |
////| StopIfSortedP |  76.78 ms |  1.458 ms |  2.879 ms |  0.21 |    0.02 |
type SorterSetGreenTest() =
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorterCount = (SorterCount.create "" 24) |> Result.ExtractOrThrow
    let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow
    let sorter16s = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
                   |> Seq.replicate (SorterCount.value sorterCount) 
    let sorterSet16 = SorterSet.fromSorters degree sorter16s


    [<Benchmark(Baseline = true)>]
    member this.CompleteSort() =
        SorterOps.CompleteSort sortableSet sorterSet16.sorters false

    [<Benchmark>]
    member this.StopIfSorted() =
        SorterOps.StopIfSorted sortableSet sorterSet16.sorters false

    [<Benchmark>]
    member this.CompleteSortP() =
        SorterOps.CompleteSort sortableSet sorterSet16.sorters true

    [<Benchmark>]
    member this.StopIfSortedP() =
        SorterOps.StopIfSorted sortableSet sorterSet16.sorters true



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


//[<MemoryDiagnoser>]
type Md5VsSha256() =
    let N = 100000
    let data = Array.zeroCreate N
    let sha256 = SHA256.Create();
    let md5 = MD5.Create()

    member this.GetData =
        data

    [<Benchmark(Baseline = true)>]
    member this.Sha256() =
        sha256.ComputeHash(data)

    [<Benchmark>]
    member this.Md5() =
        md5.ComputeHash(data)


//[<MemoryDiagnoser>]
type RandoBench() =
    let seed = RandomSeed.create "" 424 |> Result.toOption
    let seed2 = RandomSeed.create "" 42 |> Result.toOption
    let randoNet = new RandomNet(seed.Value) :> IRando
    let randoLcg = new RandomLcg(seed.Value) :> IRando
    let randoNet2 = new RandomNet(seed2.Value) :> IRando
    let randoLcg2 = new RandomLcg(seed2.Value) :> IRando

    [<Benchmark(Baseline = true)>]
    member this.NetI() =
        randoNet.NextUInt

    [<Benchmark>]
    member this.NetL() =
        randoNet.NextULong

    //[<Benchmark>]
    //member this.Lcg() =
    //    Rando.NextGuid randoLcg

    //[<Benchmark>]
    //member this.Net2() =
    //    Rando.NextGuid2 randoNet randoNet2

    //[<Benchmark>]
    //member this.Lcg2() =
    //    Rando.NextGuid2 randoLcg randoLcg2

    [<Benchmark>]
    member this.LcgUInt() =
        randoLcg2.NextUInt

    [<Benchmark>]
    member this.LcgLong() =
        randoLcg2.NextULong
