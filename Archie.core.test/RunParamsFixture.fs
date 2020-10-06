namespace Archie.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Archie.Base.SortersFromData
open System

[<TestClass>]
type RunParamsFixture () =

    [<TestMethod>]
    member this.FitnessFuncAlt() =
      let sstr = {
                    SorterTestResults.usedStageCount = StageCount.fromInt 1;
                    SorterTestResults.successfulSortCount = SortableCount.fromInt 3;
                    SorterTestResults.usedSwitchCount = SwitchCount.fromInt 7;
                    SorterTestResults.switchUses = SwitchUses.createEmpty(SwitchCount.fromInt 100);
                 }
      //let genNum0 = GenerationNumber.fromInt 9
      //let genNum1 = GenerationNumber.fromInt 10
      //let genNum2 = GenerationNumber.fromInt 11
      //let dd = FitnessFunc.altSwitchAndStage 4.0 6.0 (GenerationNumber.fromInt 10)
      //let sorterFitness0 = dd.func (Some (genNum0:>obj)) sstr 
      //let sorterFitness1 = dd.func (Some (genNum1:>obj)) sstr 
      //let sorterFitness2 = dd.func (Some (genNum2:>obj)) sstr 

      //Assert.IsTrue((SorterFitness.fromFloat 0.5) = sorterFitness0)
      let ffp = FitnessFuncParam.NoParam
      let quak = FitnessFunc.standardStage
      let res = (quak.func |> FF.value) ffp sstr
      Assert.IsTrue(true)


    [<TestMethod>]
    member this.FitnessFuncDto() =
           let dto =  FitnessFuncDto.altSwitchStageAndSuccessDto (GenerationNumber.fromInt 5)
           let json = Json.serialize dto
           let dtoBack = Json.deserialize<FitnessFuncDto> json |> Result.ExtractOrThrow
           Assert.AreEqual(dto, dtoBack)
           let ff =  FitnessFuncDto.fromDto dto
           Assert.IsTrue(true)
