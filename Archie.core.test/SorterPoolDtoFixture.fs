namespace Archie.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open System

[<TestClass>]
type SorterPoolDtoFixture () =

    [<TestMethod>]
    member this.SorterPoolMemberDto() =
        let rootSorter = RefSorter.CreateRefSorter RefSorter.Degree8Prefix3 |> Result.ExtractOrThrow
        let spmRoot = SorterPoolMemberF.makeRoot (Guid.NewGuid()) rootSorter None None

        let initSorter = RefSorter.CreateRefSorter RefSorter.Degree8 |> Result.ExtractOrThrow

        let dtoRoot = spmRoot |> SorterPoolMemberDto.toDto
        let spmRootBack = dtoRoot |> SorterPoolMemberDto.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(spmRoot, spmRootBack)

        let spmInit = SorterPoolMemberF.toInitiate (fun s->initSorter) spmRoot (GenerationNumber.fromInt 2)
        let dtoInit = spmInit |> SorterPoolMemberDto.toDto
        let spmInitBack = dtoInit |> SorterPoolMemberDto.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(spmInit, spmInitBack)

        let swUses = SwitchUses.create (SwitchCount.fromInt 22) (Array.init 22 (fun i->i)) |> Result.ExtractOrThrow
        let testResults = 
            {
               SorterTestResults.successfulSortCount = SortableCount.fromInt 11;
               SorterTestResults.switchUses = swUses;
               SorterTestResults.usedStageCount = StageCount.fromInt 33;
               SorterTestResults.usedSwitchCount = SwitchCount.fromInt 44;
            }

        let spmMeas = SorterPoolMemberF.toMeasured spmInit (fun s->testResults)
        let dtoMeas = spmMeas |> SorterPoolMemberDto.toDto
        let spmMeasBack = dtoMeas |> SorterPoolMemberDto.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(spmMeas, spmMeasBack)

        let ff2 = FitnessFunc.standardStage
       // let spmEval = SorterPoolMemberF.toEvaluated spmMeas (FitnessFunc.standardStage 1.0) (GenerationNumber.fromInt 3)
        let spmEval = SorterPoolMemberF.toEvaluated  
                                spmMeas 
                                ff2 
                                (FitnessFuncParam.Gen (GenerationNumber.fromInt 3))

        let dtoEval = spmEval |> SorterPoolMemberDto.toDto
        let spmEvalBack = dtoEval |> SorterPoolMemberDto.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(spmEval, spmEvalBack)

        let archiver = fun spm -> spm |> ignore
        let spmArchived = SorterPoolMemberF.toArchived archiver spmEval 
        let dtoArchived = spmArchived |> SorterPoolMemberDto.toDto
        let spmArchivedBack = dtoArchived |> SorterPoolMemberDto.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(spmArchived, spmArchivedBack)
