namespace Archie.core.test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Archie.Base.SorterParts
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
         let randSorterGen = RandSorterGeneration.Switch switchCount
         let randoLcg = new RandomLcg(seed) :> IRando

         let sorterSet = SorterSet.createRandom degree randSorterGen
                                    sorterCount randoLcg

         let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow

         let res = SorterOps.CompleteSort sortableSet sorterSet.sorters false
         let goodies = res |> Array.filter(fun (_, _, r) -> r=sortableSet.count)
         let duke = goodies.Length
         Assert.IsTrue (duke > 0)


    [<TestMethod>]
     member this.RandomStagePackedSorterTR() =
         let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
         let stageCount = (StageCount.create "" 110) |> Result.ExtractOrThrow
         let randSorterGen = RandSorterGeneration.Stage stageCount
         let sorterCount = (SorterCount.create "" 10) |> Result.ExtractOrThrow
         let seed = RandomSeed.create "" 4124 |> Result.ExtractOrThrow
         let randoLcg = new RandomLcg(seed) :> IRando

         let sorterSet = SorterSet.createRandom degree randSorterGen
                                    sorterCount randoLcg

         let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow

         let res = SorterOps.StopIfSorted sortableSet sorterSet.sorters false
         let goodies = res |> Array.filter(fun (_, _, r) -> r=sortableSet.count)
         let duke = goodies.Length
         Assert.IsTrue (duke > 0)


    [<TestMethod>]
     member this.RunSorterSetOnSortableSetTB() =
         let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
         let switchCount = (SwitchCount.create "" 1600) |> Result.ExtractOrThrow
         let randSorterGen = RandSorterGeneration.Switch switchCount
         let sorterCount = (SorterCount.create "" 200) |> Result.ExtractOrThrow
         let seed = RandomSeed.create "" 41324 |> Result.ExtractOrThrow
         let randoLcg = new RandomLcg(seed) :> IRando

         let sorterSet = SorterSet.createRandom degree randSorterGen
                                    sorterCount randoLcg

         let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow
         let res = SorterOps.CompleteSort sortableSet sorterSet.sorters false
         let goodies = res |> Array.map(fun (_, u, _) -> u|> SwitchUses.entropyBits )
         goodies |> Array.iter(fun v-> Debug.WriteLine(sprintf "%A" v))
         let goodies2 = res |> Array.map(fun (_, u, _) -> u|> SwitchUses.getSwitchUseCount |> Result.ExtractOrThrow )
         goodies2 |> Array.iter(fun v-> Debug.WriteLine(sprintf "%A" v))
         Assert.IsTrue (true)


    [<TestMethod>]
     member this.GetEntropyBitsForRandom() =
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let stageCount = (StageCount.create "" 200) |> Result.ExtractOrThrow
        let randSorterGen = RandSorterGeneration.Stage stageCount
        let sorterCount = (SorterCount.create "" 10) |> Result.ExtractOrThrow
        let seed = RandomSeed.create "" 4124 |> Result.ExtractOrThrow
        let randoLcg = new RandomLcg(seed) :> IRando

        let sorterSet = SorterSet.createRandom degree randSorterGen
                                sorterCount randoLcg

        let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow

        let res = SorterOps.CompleteSort sortableSet sorterSet.sorters false
        let goodies = res |> Array.map(fun (_, u, _) -> u |> SwitchUses.entropyBits)
        Debug.WriteLine(sprintf "%A" goodies)
        let goodies2 = res |> Array.map(fun (_, u, _) -> u |> SwitchUses.getSwitchUseCount |> Result.ExtractOrThrow ) 
        Debug.WriteLine(sprintf "%A" goodies2)
        Assert.IsTrue (true)


    [<TestMethod>]
    member this.GetEntropyBitsForClassic() =
        let sorter = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
        let sorte2 = RefSorter.CreateRefSorter RefSorter.End16 |> Result.ExtractOrThrow
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let sorterSet = SorterSet.fromSorters degree (seq {sorter; sorte2})
        let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow

        let res = SorterOps.CompleteSort sortableSet sorterSet.sorters false
        let goodies = res |> Array.map(fun (_, u, _) -> u |> SwitchUses.entropyBits)
        goodies |> Array.iter(fun a -> Console.WriteLine(sprintf "%A" a))
        Assert.IsTrue (true)


    [<TestMethod>]
    member this.OverStuff() =
        //let sorter = RefSorter.CreateRefSorter RefSorter.Green16m |> Result.ExtractOrThrow
        let sorter = RefSorter.CreateRefSorter RefSorter.End16m |> Result.ExtractOrThrow
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let sorterSet = SorterSet.fromSorters degree (seq {sorter; sorter})
        let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow

        let srtr, su, res = SorterOps.SortAllTR sorter sortableSet

        Assert.IsTrue (res>0)