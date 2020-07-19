namespace Archie.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Archie.Base.Sorting
open Archie.Base.Combinatorics_Types
open Archie.Base.SortersFromData

[<TestClass>]
type SorterFixture () =

    [<TestMethod>]
    member this.RandomSorterDef() =
        let degree = 6 |> Degree.create "" |> Result.toOption
        let sorterLen = 20 |> SwitchCount.create "" |> Result.toOption
        let sortableCount = 4
        let seed = 123 |> RandomSeed.create "" |> Result.toOption
        let rnd = new RandomLcg(seed.Value)

        let sorterDef = SorterDef.CreateRandom degree.Value sorterLen.Value rnd
        Assert.IsTrue (true)


    [<TestMethod>]
    member this.RunSorterDef() =
        let degree = 16 |> Degree.create "" |> Result.toOption
        let sorterLen = 800 |> SwitchCount.create "" |> Result.toOption
        let sortableCount = 4000
        let seed = 123 |> RandomSeed.create "" |> Result.toOption
        let rnd = new RandomLcg(seed.Value)

        let SortableGen (degree:Degree) (rnd:IRando) (count:int) =
            Permutation.CreateRandom rnd (Degree.value degree)
            |> Seq.map(fun i -> Permutation.value i )
            |> Seq.take count

        let sorterDef = SorterDef.CreateRandom degree.Value sorterLen.Value rnd
        let startPos = 0
        let switchTracker = SwitchTracker.Make sorterDef.switchCount
        let (res, switchTrack) = Sorter2.UpdateSwitchTrackerAndTest
                                        sorterDef
                                        switchTracker 
                                        startPos 
                                        (SortableGen degree.Value rnd sortableCount)
        Assert.IsTrue(res>=0)
        Assert.IsTrue (switchTrack.Value.Length > 0)


    [<TestMethod>]
    member this.RunSorterDef16T() =
        let sorter = RefSorter.CreateRefSorter RefSorter.Green16
        let res = SorterT.EvalSorterT sorter
        Assert.IsTrue(fst res)


    [<TestMethod>]
    member this.RunSorterDef16() =
        let sorter = RefSorter.CreateRefSorter RefSorter.Green16
        let res = Sorter.EvalSorter sorter
        Assert.IsTrue(res)