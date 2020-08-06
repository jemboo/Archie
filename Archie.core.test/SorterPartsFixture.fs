namespace Archie.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Archie.Base.SorterParts

[<TestClass>]
type SortingFixture () =

    [<TestMethod>]
    member this.MakeRandomSwitches() =
        let degree = Degree.create "" 10 |> Result.ExtractOrThrow
        let shortLength = SwitchCount.create "" 100 |> Result.ExtractOrThrow
        let longLength = SwitchCount.create "" 500 |> Result.ExtractOrThrow
        let seed = RandomSeed.create "" 424 |> Result.ExtractOrThrow
        let randoLcg = new RandomNet(seed)

        let swGen = Switch.RandomSwitchesOfDegree degree randoLcg

        let genArray = swGen |> Seq.take 1000 |> Seq.toArray
        let lowMin = genArray |> Seq.map (fun s->s.hi) |> Seq.min
        let lowMax = genArray |> Seq.map (fun s->s.hi) |> Seq.max
        let hiMin = genArray |> Seq.map (fun s->s.hi) |> Seq.min
        let hiMax = genArray |> Seq.map (fun s->s.hi) |> Seq.max

        Assert.AreEqual(0, lowMin)
        Assert.AreEqual(0, hiMin)
        Assert.AreEqual((Degree.value degree) - 1, lowMax)
        Assert.AreEqual((Degree.value degree) - 1, hiMax)
        Assert.AreEqual(100, (SwitchCount.value shortLength))

        Assert.IsTrue(true);


    [<TestMethod>]
    member this.RandSwitchUniformity() =
        let degree = Degree.create "" 10 |> Result.ExtractOrThrow
        let shortLength = SwitchCount.create "" 100 |> Result.ExtractOrThrow
        let longLength = SwitchCount.create "" 500 |> Result.ExtractOrThrow
        let seed = RandomSeed.create "" 424 |> Result.ExtractOrThrow
        let randoLcg = new RandomNet(seed)

        let swGen = Switch.RandomSwitchesOfDegree degree randoLcg
        let genArray = swGen |> Seq.take 55000 |> Seq.toArray
        let histo = Utils.histogram (fun i->i) genArray
    
        Assert.IsTrue(true);


    [<TestMethod>]
    member this.MakeRandomSorter() =
        let degree = Degree.create "" 16 |> Result.ExtractOrThrow
        let switchCount = SwitchCount.create "" 100 |> Result.ExtractOrThrow
        let seed = RandomSeed.create "" 424 |> Result.ExtractOrThrow

        let randoLcg = new RandomNet(seed)
        let sorter = Sorter.CreateRandom degree switchCount randoLcg
        Assert.AreEqual(sorter.switches.Length, (SwitchCount.value switchCount))

        let randoLcg2 = new RandomNet(seed)
        let sorter2 = Sorter.CreateRandom degree switchCount randoLcg2
        Assert.AreEqual(sorter, sorter2)

        let seed2 = RandomSeed.create "" 425 |> Result.ExtractOrThrow
        let randoLcg3 = new RandomNet(seed2)
        let sorter3 = Sorter.CreateRandom degree switchCount randoLcg3
        Assert.AreNotEqual(sorter, sorter3)

        Assert.IsTrue(true);
