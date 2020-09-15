namespace Archie.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Archie.Base.SortersFromData
open System

[<TestClass>]
type RunParamsFixture () =

    [<TestMethod>]
    member this.IndexedRandomData() =
       let rg = RngGen.createLcg 123
       let seeds = RandoCollections.IndexedSeedGen rg
                   |> Seq.take(10) |> Seq.toArray

       let guids = RandoCollections.IndexedGuidGen rg None
                    |> Seq.take(10) |> Seq.toArray

       Assert.IsTrue(seeds.Length = 10)
       Assert.IsTrue(guids.Length = 10)


    [<TestMethod>]
    member this.FitnessFuncAlt() =
      let sstr = {
                    StandardSorterTestResults.stageUseCount = StageCount.fromInt 1;
                    StandardSorterTestResults.successfulSortCount = SortableCount.fromInt 3;
                    StandardSorterTestResults.usedSwitchCount = SwitchCount.fromInt 7;
                    StandardSorterTestResults.switchUses = SwitchUses.create(SwitchCount.fromInt 100);
                 }
      let genNum0 = GenerationNumber.fromInt 9
      let genNum1 = GenerationNumber.fromInt 10
      let genNum2 = GenerationNumber.fromInt 11
      let dd = FitnessFunc.altSwitchAndStage 4.0 6.0 (GenerationNumber.fromInt 10)
      let sorterFitness0 = dd.func sstr genNum0
      let sorterFitness1 = dd.func sstr genNum1
      let sorterFitness2 = dd.func sstr genNum2

      Assert.IsTrue((SorterFitness.fromFloat 0.5) = sorterFitness0)
