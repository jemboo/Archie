namespace Archie.core.test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Archie.Base.Sorting
open BenchmarkDotNet.Attributes
open Archie.Base.SortersFromData
open System

//|           Method |     Mean |    Error |   StdDev | Ratio | RatioSD |
//|----------------- |---------:|---------:|---------:|------:|--------:|
//|             S16X | 32.14 ms | 0.631 ms | 0.821 ms |  1.00 |    0.00 |
//|          SortAll | 18.61 ms | 0.121 ms | 0.107 ms |  0.58 |    0.01 |
//| SortAllWithSlice | 19.70 ms | 0.283 ms | 0.251 ms |  0.61 |    0.02 |
//| S16SortAndTrackR | 20.05 ms | 0.388 ms | 0.344 ms |  0.62 |    0.02 |
//|    SortAndTrackT | 18.75 ms | 0.235 ms | 0.220 ms |  0.58 |    0.02 |

type SorterFullTestSlice() =
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorter16 = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
    let baseArray = IntBits.AllBinaryTestCasesArray (Degree.value degree)
                    |> Array.collect(fun a -> a)
    let backArray = Array.zeroCreate baseArray.Length
    let res = Array.Copy(baseArray, backArray, baseArray.Length)

    let sortable2Array = Sortable2.FromIntArray degree baseArray |> Seq.toArray

    [<Benchmark(Baseline = true)>]
    member this.S16X() =
        let sortableCases = Sortable.AllBinary degree |> Seq.toArray
        SorterX.SortTheCases sorter16 sortableCases true

    [<Benchmark>]
    member this.SortAll() =
        Sorter2.SortAll sorter16 sortable2Array |> ignore
        Array.Copy(backArray, baseArray, baseArray.Length)

    [<Benchmark>]
    member this.SortAllWithSlice() =
        Sorter2.SortAllWithSlice sorter16 sortable2Array |> ignore
        Array.Copy(backArray, baseArray, baseArray.Length)

    [<Benchmark>]
    member this.S16SortAndTrackR() =
        Sorter2.SortAllAndTrackR sorter16 sortable2Array |> ignore
        Array.Copy(backArray, baseArray, baseArray.Length)

    [<Benchmark>]
    member this.SortAndTrackT() =
        let res = Sorter2.SortAllAndTrackT sorter16 sortable2Array
        Array.Copy(backArray, baseArray, baseArray.Length)
        res

    [<Benchmark>]
    member this.SortAndTrackTB() =
        let res = Sorter2.SortAllAndTrackTB sorter16 sortable2Array
        Array.Copy(backArray, baseArray, baseArray.Length)
        res

    //let baseArray = IntBits.AllBinaryTestCasesArray (Degree.value degree)
    //                |> Array.collect(fun a -> a)
    
    //let sortableSeq = Sortable2.FromIntArray degree baseArray
    //let sortableArray = sortableSeq |> Seq.toArray



//| Method |     Mean |    Error |   StdDev | Ratio | RatioSD |
//|------- |---------:|---------:|---------:|------:|--------:|
//|  S16xQ | 30.57 ms | 0.606 ms | 1.759 ms |  1.00 |    0.00 |
//|  S16xA | 29.77 ms | 0.613 ms | 1.808 ms |  0.98 |    0.08 |
//|   S16t | 30.09 ms | 0.598 ms | 1.676 ms |  0.99 |    0.07 |
//| S16srt | 26.66 ms | 0.952 ms | 2.808 ms |  0.88 |    0.10 |
type SorterFullTestXvT() =
    let degree = (Degree.create "" 16 ) |>Result.ExtractOrThrow
    let sorter16 = RefSorter.CreateRefSorter RefSorter.End16 |> Result.ExtractOrThrow
    

    [<Benchmark(Baseline = true)>]
    member this.S16xQ() =
        let sortableCases = Sortable.AllBinary degree  |> Seq.toArray
        SorterX.SortTheCases sorter16 sortableCases true

    [<Benchmark>]
    member this.S16xA() =
        let sortableCases = Sortable.AllBinary degree  |> Seq.toArray
        SorterX.SortTheCases sorter16 sortableCases false

    [<Benchmark>]
    member this.S16t() =
        let sortableCases = Sortable.AllBinary degree  |> Seq.toArray
        SorterR.SortTheCasesAndTrack sorter16 sortableCases
        
    [<Benchmark>]
    member this.S16srt() =
        let sortableCases = Sortable.AllBinary degree  |> Seq.toArray
        sortableCases

//|           Method |     Mean |    Error |   StdDev | Ratio |
//|----------------- |---------:|---------:|---------:|------:|
//|    ArraySequence | 22.64 ms | 0.287 ms | 0.254 ms |  1.00 |
//| SortableWithCopy | 13.33 ms | 0.037 ms | 0.031 ms |  0.59 |
//|    ArrayWithCopy | 15.65 ms | 0.228 ms | 0.213 ms |  0.69 |
//|  SortableInPlace | 13.20 ms | 0.203 ms | 0.180 ms |  0.58 |
//|     ArrayInPlace | 15.59 ms | 0.072 ms | 0.060 ms |  0.69 |
type SorterFullTestXvsX2() =
    let degree = (Degree.create "" 16 ) |>Result.ExtractOrThrow
    let sorter = RefSorter.CreateRefSorter RefSorter.End16|> Result.ExtractOrThrow
    let sortableCases = Sortable.AllBinary degree  |> Seq.toArray
    let sortableCasesC = Sortable.AllBinary degree  |> Seq.toArray
    
    member this.SortableCases() =
        sortableCases
    member this.SortableCasesC() =
        sortableCasesC

    [<Benchmark(Baseline = true)>]
    member this.ArraySequence() =
        SorterX.SortAllBinaries sorter |> ignore



//| Method |      Mean |     Error |    StdDev | Ratio | RatioSD |
//|------- |----------:|----------:|----------:|------:|--------:|
//|  Short |  27.20 ns |  0.278 ns |  0.260 ns |  1.00 |    0.00 |
//|   Long | 914.85 ns | 18.011 ns | 20.019 ns | 33.64 |    0.81 |

type ArrayCopy() =

    let arroyoA = Array.create 100000 5
    let arroyoB = Array.create 100000 6

    let arroyoAw = Array.create 640000 5
    let arroyoBw = Array.create 640000 6

    //let spanA = new Span<int>(arroyoA)
    //let spanB = new Span<int>(arroyoB)

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
        Array.Copy(arroyoA, arroyoB, 100000)

    [<Benchmark>]
    member this.Long() =
        Array.Copy(arroyoAw, arroyoBw, 640000)





[<TestClass>]
type BenchmarkFixture () =

    let sorter16 = RefSorter.CreateRefSorter RefSorter.End16 |> Result.ExtractOrThrow
    let sorterTrim = result {
                            let! fullLen = RefSorter.CreateRefSorter RefSorter.End16
                            let! trimLen = SwitchCount.create "" 59
                            return! SorterDef.TrimLength fullLen trimLen
                        } |> Result.ExtractOrThrow

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
         //let rS = res.SortAndTrackT()
         //Console.WriteLine (sprintf"SortAndTrackT: %A" rS)

         let useTrack, lastTrack = res.SortAndTrackTB()
         Console.WriteLine (sprintf "useTrack: %A" useTrack)
         Console.WriteLine (sprintf "lastTrack: %A" lastTrack)
         Assert.IsTrue (true)

