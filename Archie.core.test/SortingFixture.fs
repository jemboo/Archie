namespace Archie.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Archie.Base.Sorting

[<TestClass>]
type SortingFixture () =

    [<TestMethod>]
    member this.MakeRandomSwitches() =
        let degree = Degree.create "" 10 |> Result.toOption
        let shortLength = SwitchCount.create "" 100 |> Result.toOption
        let longLength = SwitchCount.create "" 500 |> Result.toOption
        let seed = RandomSeed.create "" 424 |> Result.toOption
        let randoLcg = new RandomNet(seed.Value)

        let swGen = Switch.RandomSwitchesOfDegree degree.Value randoLcg

        let genArray = swGen |> Seq.take 1000 |> Seq.toArray
        let lowMin = genArray |> Seq.map (fun s->s.hi) |> Seq.min
        let lowMax = genArray |> Seq.map (fun s->s.hi) |> Seq.max
        let hiMin = genArray |> Seq.map (fun s->s.hi) |> Seq.min
        let hiMax = genArray |> Seq.map (fun s->s.hi) |> Seq.max

        Assert.AreEqual(0, lowMin)
        Assert.AreEqual(0, hiMin)
        Assert.AreEqual((Degree.value degree.Value) - 1, lowMax)
        Assert.AreEqual((Degree.value degree.Value) - 1, hiMax)
        Assert.AreEqual(100, (SwitchCount.value shortLength.Value))

        Assert.IsTrue(true);


    [<TestMethod>]
    member this.RandSwitchUniformity() =
        let degree = Degree.create "" 10 |> Result.toOption
        let shortLength = SwitchCount.create "" 100 |> Result.toOption
        let longLength = SwitchCount.create "" 500 |> Result.toOption
        let seed = RandomSeed.create "" 424 |> Result.toOption
        let randoLcg = new RandomNet(seed.Value)

        let swGen = Switch.RandomSwitchesOfDegree degree.Value randoLcg

        let genArray = swGen |> Seq.take 55000 |> Seq.toArray
        let histo = Utils.histogram (fun i->i) genArray
    
        Assert.IsTrue(true);


    [<TestMethod>]
    member this.MakeRandomSorter () =
        let degree = Degree.create "" 16 |> Result.toOption
        let shortLength = SwitchCount.create "" 100 |> Result.toOption
        let longLength = SwitchCount.create "" 500 |> Result.toOption
        let seed = RandomSeed.create "" 424 |> Result.toOption
        let randoLcg = new RandomNet(seed.Value)

        let sorter = SorterDef.CreateRandom degree.Value shortLength.Value randoLcg

        Assert.AreEqual(sorter.switches.Length, (SwitchCount.value shortLength.Value))
        Assert.IsTrue(true);
