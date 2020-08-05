namespace Archie.core.test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Archie.Base.SorterParts
open BenchmarkDotNet.Attributes
open Archie.Base.SortersFromData
open System
open System.Diagnostics


[<TestClass>]
type SortingRunFixture() =

    [<TestMethod>]
     member this.Flatten() =
         let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
         let stageCount = (StageCount.create "" 110) |> Result.ExtractOrThrow
         let sorterCount = (SorterCount.create "" 10) |> Result.ExtractOrThrow
         let seed = RandomSeed.create "" 4124 |> Result.ExtractOrThrow
         let randoLcg = new RandomLcg(seed) :> IRando

         let sorterSet = SorterSet.createRandomStagePacked degree stageCount
                                    sorterCount randoLcg

         let sortableSet = SortableSet.AllBinary degree |> Result.ExtractOrThrow

         let res = SortingRun.RunSorterSetOnSortableSetTR sortableSet sorterSet
         let goodies = res |> Array.filter(fun t -> snd t)
         let duke = goodies.Length
         Assert.IsTrue (duke > 0)


    [<TestMethod>]
     member this.Flatten2() =
         let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
         let switchCount = (SwitchCount.create "" 880) |> Result.ExtractOrThrow
         let sorterCount = (SorterCount.create "" 10) |> Result.ExtractOrThrow
         let seed = RandomSeed.create "" 4124 |> Result.ExtractOrThrow
         let randoLcg = new RandomLcg(seed) :> IRando

         let sorterSet = SorterSet.createRandom degree switchCount
                                    sorterCount randoLcg

         let sortableSet = SortableSet.AllBinary degree |> Result.ExtractOrThrow

         let res = SortingRun.RunSorterSetOnSortableSetTR sortableSet sorterSet
         let goodies = res |> Array.filter(fun t -> snd t)
         let duke = goodies.Length
         Assert.IsTrue (duke > 0)



    [<TestMethod>]
     member this.Flatten3() =
         let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
         let switchCount = (SwitchCount.create "" 1600) |> Result.ExtractOrThrow
         let sorterCount = (SorterCount.create "" 200) |> Result.ExtractOrThrow
         let seed = RandomSeed.create "" 41324 |> Result.ExtractOrThrow
         let randoLcg = new RandomLcg(seed) :> IRando

         let sorterSet = SorterSet.createRandom degree switchCount
                                    sorterCount randoLcg

         let sortableSet = SortableSet.AllBinary degree |> Result.ExtractOrThrow

         let res = SortingRun.RunSorterSetOnSortableSetTB sortableSet sorterSet
         let goodies = res |> Array.map(fun r -> (fst r)|> SwitchTracker.EntropyBits )
         goodies |> Array.iter(fun v-> Debug.WriteLine(sprintf "%A" v))
         let goodies2 = res |> Array.map(fun r -> (snd r)|> SwitchTracker.UseCount)
         goodies2 |> Array.iter(fun v-> Debug.WriteLine(sprintf "%A" v))
         Assert.IsTrue (true)



    [<TestMethod>]
     member this.Flatten4() =
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let stageCount = (StageCount.create "" 200) |> Result.ExtractOrThrow
        let sorterCount = (SorterCount.create "" 10) |> Result.ExtractOrThrow
        let seed = RandomSeed.create "" 4124 |> Result.ExtractOrThrow
        let randoLcg = new RandomLcg(seed) :> IRando

        let sorterSet = SorterSet.createRandomStagePacked degree stageCount
                                sorterCount randoLcg

        let sortableSet = SortableSet.AllBinary degree |> Result.ExtractOrThrow

        let res = SortingRun.RunSorterSetOnSortableSetTB sortableSet sorterSet
        let goodies = res |> Array.map(fun r -> (fst r)|> SwitchTracker.EntropyBits)
        Debug.WriteLine(sprintf "%A" goodies)
        let goodies2 = res |> Array.map(fun r -> (snd r)|> SwitchTracker.UseCount)
        Debug.WriteLine(sprintf "%A" goodies2)
        Assert.IsTrue (true)



    [<TestMethod>]
        member this.Flatten5() =
            let sorter = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
            let sorte2 = RefSorter.CreateRefSorter RefSorter.End16 |> Result.ExtractOrThrow
            let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
            let sorterSet = SorterSet.fromSorters degree (seq {sorter; sorte2})
            let sortableSet = SortableSet.AllBinary degree |> Result.ExtractOrThrow

            let res = SortingRun.RunSorterSetOnSortableSetTB sortableSet sorterSet
            let goodies = res |> Array.map(fun r -> (fst r) |> SwitchTracker.EntropyBits)
            goodies |> Array.iter(fun a -> Console.WriteLine(sprintf "%A" a))
            Assert.IsTrue (true)