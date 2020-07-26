namespace Archie.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Archie.Base.Sorting
open Archie.Base.SortersFromData

[<TestClass>]
type SorterFixture () =

    [<TestMethod>]
    member this.RandomSorterDef() =
        let degree = 6 |> Degree.create "" |> Result.ExtractOrThrow
        let sorterLen = 20 |> SwitchCount.create "" |> Result.ExtractOrThrow
        let sortableCount = 4
        let seed = 123 |> RandomSeed.create "" |> Result.ExtractOrThrow
        let rnd = new RandomLcg(seed)
        let sorterDef = SorterDef.CreateRandom degree sorterLen rnd
        Assert.IsTrue (true)


    [<TestMethod>]
    member this.RunSorterDef() =
        let degree = 16 |> Degree.create "" |> Result.ExtractOrThrow
        let sorterLen = 800 |> SwitchCount.create "" |> Result.ExtractOrThrow
        let sortableCount = 4000
        let seed = 123 |> RandomSeed.create "" |> Result.ExtractOrThrow
        let rnd = new RandomLcg(seed)

        let sorterDef = SorterDef.CreateRandom degree sorterLen rnd
        let startPos = 0
        let switchTracker = SwitchTracker.create sorterDef.switchCount
        let (res, switchTrack) = SorterT.CollectFailsAndTracker sorterDef None None 
        //let weights = 
        Assert.IsTrue(res.Length > 0)
        Assert.IsTrue ((SwitchTracker.weights switchTrack) |> Array.sum > 0)



    [<TestMethod>]
    member this.RunSorterDef16() =
        //let sorter = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
        //let res = Sorter.EvalSorter sorter
        Assert.IsTrue(true)


    [<TestMethod>]
    member this.RunSorterDef16Trim() =
        let sorter = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
        let sorterTrim = result {
                                let! fullLen = RefSorter.CreateRefSorter RefSorter.Green16
                                let! trimLen = SwitchCount.create "" 58
                                return! SorterDef.TrimLength fullLen trimLen
                            } |> Result.ExtractOrThrow

        let resTrim = SorterC.CollectFails sorterTrim None None |> Seq.toArray

        Assert.IsTrue(resTrim.Length > 0)
