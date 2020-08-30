namespace Archie.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Archie.Base.SorterParts
open Archie.Base.SortersFromData

[<TestClass>]
type SortingFixture () =

    [<TestMethod>]
    member this.MakeRandomSwitches() =
        let degree = Degree.create "" 10 |> Result.ExtractOrThrow
        let shortLength = SwitchCount.create "" 100 |> Result.ExtractOrThrow
        let rnd = Rando.LcgFromSeed 424
        let swGen = Switch.randomSwitchesOfDegree degree rnd

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


    [<TestMethod>]
    member this.RandSwitchUniformity() =
        let degree = Degree.create "" 10 |> Result.ExtractOrThrow
        let rnd = Rando.LcgFromSeed 424

        let swGen = Switch.randomSwitchesOfDegree degree rnd
        let genArray = swGen |> Seq.take 55000 |> Seq.toArray
        let histo = Utils.histogram (fun i->i) genArray
    
        Assert.IsTrue(true);


    [<TestMethod>]
    member this.MakeRandomSorter() =
        let rnd = Rando.LcgFromSeed 424
        let rnd2 = Rando.LcgFromSeed 424
        let rnd3 = Rando.LcgFromSeed 7721

        let degree = Degree.create "" 16 |> Result.ExtractOrThrow
        let switchCount = SwitchCount.create "" 100 |> Result.ExtractOrThrow
        let randSorterGen = RndSorterGen.Switch switchCount
        let sorter = Sorter.createRandom degree randSorterGen rnd
        Assert.AreEqual(sorter.switches.Length, (SwitchCount.value switchCount))
        let sorter2 = Sorter.createRandom degree randSorterGen rnd2
        Assert.AreEqual(sorter, sorter2)
        let sorter3 = Sorter.createRandom degree randSorterGen rnd3
        Assert.AreNotEqual(sorter, sorter3)


    [<TestMethod>]
    member this.RandomSorterProperties() =
        let degree = 6 |> Degree.create "" |> Result.ExtractOrThrow
        let switchCount = 20 |> SwitchCount.create "" |> Result.ExtractOrThrow
        let randSorterGen = RndSorterGen.Switch switchCount
        let seed = 123 |> RandomSeed.create "" |> Result.ExtractOrThrow
        let rnd = new RandomLcg(seed)
        let sorter = Sorter.createRandom degree randSorterGen rnd
        Assert.AreEqual (sorter.switchCount, switchCount)
        Assert.AreEqual (sorter.degree, degree)


    [<TestMethod>]
    member this.GetUsedSwitches() =
        let sorter = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
        let switchCount = sorter.switches.Length |> SwitchCount.create "" |> Result.ExtractOrThrow
        let switchUses = SwitchUses.create switchCount
        let weights = SwitchUses.getWeights switchUses
        weights.[0] <- 1
        weights.[3] <- 1
        weights.[weights.Length - 1] <- 1
        let usedSwitches = SwitchUses.getUsedSwitches switchUses sorter |> Result.ExtractOrThrow
        Assert.AreEqual(usedSwitches.Length, 3)
        Assert.IsTrue (true)


    [<TestMethod>]
    member this.GetRefinedStageCount() =
        let sorter = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
        let switchCount = sorter.switches.Length |> SwitchCount.create "" |> Result.ExtractOrThrow
        let switchUses = SwitchUses.create switchCount
        let weights = SwitchUses.getWeights switchUses
        weights.[0] <- 1
        weights.[3] <- 1
        weights.[weights.Length - 1] <- 1
        let stagecount = SwitchUses.getRefinedStageCount switchUses sorter |> Result.ExtractOrThrow
        Assert.AreEqual((StageCount.value stagecount), 1)
        Assert.IsTrue (true)


    [<TestMethod>]
    member this.SorterTrimLength() =
        let trimCount = 58 |> SwitchCount.create "" |> Result.ExtractOrThrow
        let degree = (Degree.create "" 16) |> Result.ExtractOrThrow
        let sorterTrim = result {
                                let! refSorter = RefSorter.CreateRefSorter RefSorter.Green16
                                return! Sorter.trimLength refSorter trimCount
                            } |> Result.ExtractOrThrow


        Assert.AreEqual (sorterTrim.switchCount, trimCount)
        Assert.AreEqual (sorterTrim.degree, degree)


    [<TestMethod>]
    member this.LastUsedIndexes() =
        let switchCount = SwitchCount.create "" 10 |> Result.ExtractOrThrow
        let dex1 = 2
        let dex2low = 3
        let dex2 = 7
        let st1 = SwitchUses.create switchCount
        let st2 = SwitchUses.create switchCount
        let st1A = SwitchUses.getWeights st1
        let st2A = SwitchUses.getWeights st2
        st1A.[dex1] <- 1
        st2A.[dex2] <- 1
        st2A.[dex2low] <- 1
        let sts = seq { st1; st2 }
        let stLT = SwitchUses.lastUsedIndexes switchCount sts
        let stLTA = SwitchUses.getWeights stLT
        Assert.AreEqual(stLTA.[dex1], 1)
        Assert.AreEqual(stLTA.[dex2], 1)
        Assert.AreEqual(stLTA.[dex2low], 0)

    [<TestMethod>]
    member this.SortableSetAllBinary() =
        let degree = 12 |> Degree.create "" |> Result.ExtractOrThrow
        let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow
        let len = sortableSet.baseArray.Length
        Assert.IsTrue(true)


    [<TestMethod>]
    member this.mutate() =
        let rnd = Rando.LcgFromSeed 424
        let mutationRate = MutationRate.create "" 0.9999 |> Result.ExtractOrThrow
        let degree = 16 |> Degree.create "" |> Result.ExtractOrThrow
        let stage = Stage.createRandom degree rnd
        let mutant = Stage.randomMutate rnd mutationRate stage
        Assert.AreEqual(stage.degree, mutant.degree)
        Assert.IsTrue(mutant <> stage)

    [<TestMethod>]
    member this.switchMutateByStage() =
        let rnd = Rando.LcgFromSeed 6241
        let mutationRate = MutationRate.create "" 1.0 |> Result.ExtractOrThrow
        let degree = 15 |> Degree.create "" |> Result.ExtractOrThrow
        let stage = Stage.createRandom degree rnd
        Console.WriteLine (SorterWriter.formatStage stage)
        seq {0..100} |> Seq.iter(fun i ->
            let mutant = Stage.randomMutate rnd mutationRate stage
            if (stage = mutant) then Console.Write("match")
            Console.Write (SorterWriter.formatStage mutant)
            Assert.AreEqual(stage.degree, mutant.degree))

    [<TestMethod>]
    member this.sorterMutateByStage() =
        let rnd = Rando.LcgFromSeed 424
        let rnd2 = Rando.LcgFromSeed 424
        let rnd3 = Rando.LcgFromSeed 7721
        let mutationRate = MutationRate.create "" 0.15 |> Result.ExtractOrThrow

        let degree = Degree.create "" 16 |> Result.ExtractOrThrow
        let stageCount = StageCount.create "" 10 |> Result.ExtractOrThrow
        let randSorterGen = RndSorterGen.Stage stageCount
        let sorter = Sorter.createRandom degree randSorterGen rnd
        Console.WriteLine (SorterWriter.formatSwitches degree sorter.switches)
        Console.WriteLine("")
        seq {0..10} |> Seq.iter(fun i ->
            let mutant = Sorter.mutateByStage mutationRate rnd sorter
            if (sorter = mutant) then Console.Write("match\n")
            Console.Write (SorterWriter.formatSwitches degree mutant.switches)
            Console.WriteLine("")
            Assert.AreEqual(sorter.degree, mutant.degree))