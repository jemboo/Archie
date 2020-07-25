namespace Archie.core.test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Archie.Base.Sorting
open BenchmarkDotNet.Attributes
open Archie.Base.SortersFromData


//| Method |      Mean |    Error |   StdDev | Ratio | RatioSD |
//|------- |----------:|---------:|---------:|------:|--------:|
//|   S16x |  47.20 ms | 0.919 ms | 1.022 ms |  1.00 |    0.00 |
//|   S16c |  53.37 ms | 1.033 ms | 1.189 ms |  1.13 |    0.03 |
//|   S16t | 111.61 ms | 2.126 ms | 2.183 ms |  2.36 |    0.08 |
type SorterFullTestCvsXvsT() =

    let sorter16 = RefSorter.CreateRefSorter RefSorter.End16 |> Result.ExtractOrThrow
    let sorterTrim = result {
                            let! fullLen = RefSorter.CreateRefSorter RefSorter.Degree18
                            let! trimLen = SwitchCount.create "" 76
                            return! SorterDef.TrimLength fullLen trimLen
                        } |> Result.ExtractOrThrow

    member this.S16x() =
        SorterX.SorterFullTestPassFail sorter16 |> ignore

    [<Benchmark>]
    member this.S16c() =
        SorterC.CollectFails sorter16 |> ignore
        
    [<Benchmark>]
    member this.S16t() =
        SorterT.CollectFailsAndTracker sorter16 |> ignore


type SorterReactionTest() =

    let sorterTrim = result {
                            let! fullLen = RefSorter.CreateRefSorter RefSorter.End16
                            let! trimLen = SwitchCount.create "" 59
                            return! SorterDef.TrimLength fullLen trimLen
                        } |> Result.ExtractOrThrow

    [<Benchmark(Baseline = true)>]
    member this.S16c() =
        SorterC.CollectFails sorterTrim
    
    [<Benchmark>]
    member this.S16t() =
        SorterT.CollectFailsAndTracker sorterTrim
        

type PrivateTrackerTest() =

    let sorter16 = RefSorter.CreateRefSorter RefSorter.End16 |> Result.ExtractOrThrow
    
    [<Benchmark(Baseline = true)>]
    member this.S16t() =
        SorterT.CollectFailsAndTracker sorter16 None None
    
    [<Benchmark>]
    member this.S16t2() =
        SorterT2.CollectFailsAndTracker sorter16 None None


type SorterGen() =
    let degree = Degree.create "" 16 |> Result.ExtractOrThrow
    let shortLength = SwitchCount.create "" 100 |> Result.ExtractOrThrow
    let longLength = SwitchCount.create "" 500 |> Result.ExtractOrThrow
    let seed = RandomSeed.create "" 424 |> Result.ExtractOrThrow
    let randoLcg = new RandomLcg(seed)

    [<Benchmark(Baseline = true)>]
    member this.Short() =
        SorterDef.CreateRandom degree longLength randoLcg

    [<Benchmark>]
    member this.Long() =
        SorterDef.CreateRandom degree longLength randoLcg




[<TestClass>]
type BenchmarkFixture () =

    [<TestMethod>]
    member this.RunSorterDefBaseline() =
        
        let sorter16 = RefSorter.CreateRefSorter RefSorter.End16 |> Result.ExtractOrThrow
        let res = SorterT2.CollectFailsAndTracker sorter16 None None
        let g = snd res
        //sd.Inline() |> ignore
        Assert.IsTrue (true)