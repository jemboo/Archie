namespace Archie.core.test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Archie.Base.Sorting
open Archie.Base.Combinatorics_Types
open BenchmarkDotNet.Attributes
open Archie.Base.SortersFromData



type RunSorterRef() =

    let sorterGreen16 = RefSorter.CreateRefSorter RefSorter.Green16
    let sorterEnd16 = RefSorter.CreateRefSorter RefSorter.End16
    let sorter15 = RefSorter.CreateRefSorter RefSorter.Degree15

    [<Benchmark(Baseline = true)>]
    member this.S16() =
        Sorter.EvalSorter sorterEnd16

    [<Benchmark>]
    member this.S16C() =
        SorterC.EvalSorter sorterEnd16


type SorterGen() =
    let degree = Degree.create "" 16 |> Result.toOption
    let shortLength = SwitchCount.create "" 100 |> Result.toOption
    let longLength = SwitchCount.create "" 500 |> Result.toOption
    let seed = RandomSeed.create "" 424 |> Result.toOption
    let randoLcg = new RandomLcg(seed.Value)

    [<Benchmark(Baseline = true)>]
    member this.Short() =
        SorterDef.CreateRandom degree.Value longLength.Value randoLcg

    [<Benchmark>]
    member this.Long() =
        SorterDef.CreateRandom degree.Value longLength.Value randoLcg


type RunSorterDef() =
    let degree = 12 |> Degree.create "" |> Result.toOption
    let sorterLen = 500 |> SwitchCount.create "" |> Result.toOption
    let sortableCount = 4000
    let seed = 123 |> RandomSeed.create "" |> Result.toOption
    let rnd = new RandomLcg(seed.Value)

    let SortableGen (degree:Degree) (rnd : IRando) (count:int) =
        Permutation.CreateRandom rnd (Degree.value degree)
        |> Seq.map(fun i -> Permutation.value i )
        |> Seq.take count

    let sorterDef = SorterDef.CreateRandom degree.Value sorterLen.Value rnd
    let startPos = 0
    let switchTracker = SwitchTracker.Make sorterLen.Value

    [<Benchmark(Baseline = true)>]
    member this.Standard() =
        SorterT.EvalSortableSeqT
            sorterDef
            switchTracker
            (SortableGen degree.Value rnd sortableCount)

    [<Benchmark>]
    member this.Inline() =
        Sorter2.UpdateSwitchTracker
            sorterDef
            switchTracker 
            startPos 
            (SortableGen degree.Value rnd sortableCount)


[<TestClass>]
type BenchmarkFixture () =

    [<TestMethod>]
    member this.RunSorterDefBaseline() =
        let sd = new RunSorterDef()
        sd.Standard() |> ignore
        sd.Inline() |> ignore
        Assert.IsTrue (true)

