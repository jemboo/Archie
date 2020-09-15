namespace Archie.core.test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open System
open System.Diagnostics


[<TestClass>]
type SorterOpsFixture() =

    [<TestMethod>]
     member this.RandomSorterTR() =
         let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
         let switchCount = (SwitchCount.create "" 880) |> Result.ExtractOrThrow
         let sorterCount = (SorterCount.create "" 10) |> Result.ExtractOrThrow
         let seed = RandomSeed.create "" 4124 |> Result.ExtractOrThrow
         let randSorterGen = SorterLength.Switch switchCount
         let randoLcg = new RandomLcg(seed) :> IRando

         let sorterSet = SorterSet.createRandom degree randSorterGen None
                                    sorterCount randoLcg

         let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow

         let res = SorterOps.GetStandardSortingResultsComplete sortableSet (UseParallel.create false) sorterSet.sorters 
         let goodies = res |> Array.filter(fun (_, r) -> SortableCount.value r.successfulSortCount =sortableSet.count)
         let duke = goodies.Length
         Assert.IsTrue (duke > 0)


    [<TestMethod>]
     member this.RandomStagePackedSorterTR() =
         let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
         let stageCount = (StageCount.create "" 110) |> Result.ExtractOrThrow
         let randSorterGen = SorterLength.Stage stageCount
         let sorterCount = (SorterCount.create "" 10) |> Result.ExtractOrThrow
         let seed = RandomSeed.create "" 4124 |> Result.ExtractOrThrow
         let randoLcg = new RandomLcg(seed) :> IRando

         let sorterSet = SorterSet.createRandom degree randSorterGen None
                                    sorterCount randoLcg

         let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow

         let res = SorterOps.GetStandardSortingResultsEager sortableSet (UseParallel.create false) sorterSet.sorters
         let goodies = res |> Array.filter(fun (_, r) -> SortableCount.value r.successfulSortCount = sortableSet.count)
         let duke = goodies.Length
         Assert.IsTrue (duke > 0)


    [<TestMethod>]
     member this.RunSorterSetOnSortableSetTB() =
         let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
         let switchCount = (SwitchCount.create "" 1600) |> Result.ExtractOrThrow
         let sorterLength = SorterLength.Switch switchCount
         let sorterCount = (SorterCount.create "" 200) |> Result.ExtractOrThrow
         let seed = RandomSeed.create "" 41324 |> Result.ExtractOrThrow
         let randoLcg = new RandomLcg(seed) :> IRando

         let sorterSet = SorterSet.createRandom degree sorterLength None
                                    sorterCount randoLcg

         let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow
         let res = SorterOps.GetStandardSortingResultsComplete sortableSet (UseParallel.create false) sorterSet.sorters
         let goodies = res |> Array.map(fun (_, r) -> r.switchUses |> SwitchUses.entropyBits )
         goodies |> Array.iter(fun v-> Debug.WriteLine(sprintf "%A" v))
         let goodies2 = res |> Array.map(fun (_, r) -> r.switchUses |> SwitchUses.getSwitchUseCount |> Result.ExtractOrThrow )
         goodies2 |> Array.iter(fun v-> Debug.WriteLine(sprintf "%A" v))
         Assert.IsTrue (true)


    [<TestMethod>]
     member this.GetEntropyBitsForRandom() =
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let stageCount = (StageCount.create "" 200) |> Result.ExtractOrThrow
        let sorterLength = SorterLength.Stage stageCount
        let sorterCount = (SorterCount.create "" 10) |> Result.ExtractOrThrow
        let seed = RandomSeed.create "" 4124 |> Result.ExtractOrThrow
        let randoLcg = new RandomLcg(seed) :> IRando

        let sorterSet = SorterSet.createRandom degree sorterLength None
                                sorterCount randoLcg

        let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow

        let res = SorterOps.GetStandardSortingResultsComplete sortableSet (UseParallel.create false) sorterSet.sorters 
        let goodies = res |> Array.map(fun (_, r) -> r.switchUses |> SwitchUses.entropyBits )
        Debug.WriteLine(sprintf "%A" goodies)
        let goodies2 = res |> Array.map(fun (_, r) -> r.switchUses |> SwitchUses.getSwitchUseCount |> Result.ExtractOrThrow )
        Debug.WriteLine(sprintf "%A" goodies2)
        Assert.IsTrue (true)


    [<TestMethod>]
    member this.GetEntropyBitsForClassic() =
        let sorter = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
        let sorte2 = RefSorter.CreateRefSorter RefSorter.End16 |> Result.ExtractOrThrow
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let sorterSet = SorterSet.fromSorters degree (seq {sorter; sorte2})
        let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow

        let res = SorterOps.GetStandardSortingResultsComplete sortableSet (UseParallel.create false) sorterSet.sorters 
        let goodies = res |> Array.map(fun (_, r) -> r.switchUses |> SwitchUses.entropyBits )
        goodies |> Array.iter(fun a -> Console.WriteLine(sprintf "%A" a))
        Assert.IsTrue (true)


    [<TestMethod>]
    member this.OverStuff() =
        //let sorter = RefSorter.CreateRefSorter RefSorter.Green16m |> Result.ExtractOrThrow
        let sorter = RefSorter.CreateRefSorter RefSorter.End16m |> Result.ExtractOrThrow
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let sorterSet = SorterSet.fromSorters degree (seq {sorter; sorter})
        let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow

        let su, res = SorterOps.SortAllComplete sorter sortableSet

        Assert.IsTrue (SortableCount.value res > 0)