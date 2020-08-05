namespace Archie.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Archie.Base.SorterParts
open Archie.Base.SortersFromData
open System

[<TestClass>]
type SorterFixture () =

    [<TestMethod>]
    member this.RandomSorterDef() =
        let degree = 6 |> Degree.create "" |> Result.ExtractOrThrow
        let sorterLen = 20 |> SwitchCount.create "" |> Result.ExtractOrThrow
        let sortableCount = 4
        let seed = 123 |> RandomSeed.create "" |> Result.ExtractOrThrow
        let rnd = new RandomLcg(seed)
        let sorterDef = Sorter.CreateRandom degree sorterLen rnd
        Assert.IsTrue (true)


    [<TestMethod>]
    member this.RunSorterDef() =
        let degree = 16 |> Degree.create "" |> Result.ExtractOrThrow
        let sorterLen = 800 |> SwitchCount.create "" |> Result.ExtractOrThrow
        let sortableCount = 4000
        let seed = 123 |> RandomSeed.create "" |> Result.ExtractOrThrow
        let rnd = new RandomLcg(seed)

        Assert.IsTrue (true)


    [<TestMethod>]
    member this.RunSorterDef16Trim() =
        let sorter = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
        let sorterTrim = result {
                                let! fullLen = RefSorter.CreateRefSorter RefSorter.Green16
                                let! trimLen = SwitchCount.create "" 58
                                return! Sorter.TrimLength fullLen trimLen
                            } |> Result.ExtractOrThrow


        Assert.IsTrue(true)


    [<TestMethod>]
    member this.SliceIt() =
        let sorter = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow


        Assert.IsTrue(true)
