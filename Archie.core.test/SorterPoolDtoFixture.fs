﻿namespace Archie.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open System

[<TestClass>]
type SorterPoolDtoFixture () =

    [<TestMethod>]
    member this.SorterPoolMember2Dto() =
        let rootSorter = RefSorter.CreateRefSorter RefSorter.Degree8Prefix3 |> Result.ExtractOrThrow
        let spmRoot = SorterPoolMember.makeRoot (Guid.NewGuid()) rootSorter None None

        let initSorter = RefSorter.CreateRefSorter RefSorter.Degree8 |> Result.ExtractOrThrow

        let dtoRoot = spmRoot |> SorterPoolMemberDto2.toDto
        let cerealRoot = Json.serialize dtoRoot
        let dtoRootBack = cerealRoot |> Json.deserialize<SorterPoolMemberDto2> |> Result.ExtractOrThrow
        let spmRootBack = dtoRootBack |> SorterPoolMemberDto2.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(spmRoot, spmRootBack)

        let spmInit = SorterPoolMember.toInitiate (fun s->initSorter) spmRoot (GenerationNumber.fromInt 2)
        let dtoInit = spmInit |> SorterPoolMemberDto2.toDto
        let cereal = Json.serialize dtoInit

        let spmDtoBack = dtoInit |> SorterPoolMemberDto2.fromDto |> Result.ExtractOrThrow
        let spmInitBack = spmDtoBack |> SorterPoolMember.attachParent spmRoot |> Result.ExtractOrThrow
        Assert.AreEqual(spmInit, spmInitBack)

        let swUses = SwitchUses.create (SwitchCount.fromInt 22) (Array.init 22 (fun i->i)) |> Result.ExtractOrThrow
        let testResults = 
            {
               SorterTestResults.successfulSortCount = SortableCount.fromInt 11;
               SorterTestResults.switchUses = swUses;
               SorterTestResults.usedStageCount = StageCount.fromInt 33;
               SorterTestResults.usedSwitchCount = SwitchCount.fromInt 44;
            }

        let spmMeas = SorterPoolMember.toMeasured spmInit (fun s->testResults) |> Result.ExtractOrThrow
        let dtoMeas = spmMeas |> SorterPoolMemberDto2.toDto 
        let spmMdtoBack = dtoMeas |> SorterPoolMemberDto2.fromDto |> Result.ExtractOrThrow
        let spmMeasBack = spmMdtoBack |> SorterPoolMember.attachParent spmRoot |> Result.ExtractOrThrow
        Assert.AreEqual(spmMeas, spmMeasBack)

        let ff2 = FitnessFunc.standardStage
        let spmEval = SorterPoolMember.toEvaluated  
                                spmMeas 
                                ff2 
                                (FitnessFuncParam.NoParam)
                                |> Result.ExtractOrThrow

        let dtoEval = spmEval |> SorterPoolMemberDto2.toDto
        let spmEvalDto = dtoEval |> SorterPoolMemberDto2.fromDto |> Result.ExtractOrThrow
        let spmEvalBack = spmEvalDto |> SorterPoolMember.attachParent spmRoot |> Result.ExtractOrThrow
        Assert.AreEqual(spmEval, spmEvalBack)

        let spmArchived = SorterPoolMember.toArchived spmEval |> Result.ExtractOrThrow
        let dtoArchived = spmArchived |> SorterPoolMemberDto2.toDto
        let spmArchivedBack = dtoArchived |> SorterPoolMemberDto2.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(spmArchived, spmArchivedBack)


    [<TestMethod>]
    member this.SorterPoolMemberDto() =
        let rootSorter = RefSorter.CreateRefSorter RefSorter.Degree8Prefix3 |> Result.ExtractOrThrow
        let spmRoot = SorterPoolMember.makeRoot (Guid.NewGuid()) rootSorter None None

        let initSorter = RefSorter.CreateRefSorter RefSorter.Degree8 |> Result.ExtractOrThrow

        let dtoRoot = spmRoot |> SorterPoolMemberDto2.toDto
        let spmRootBack = dtoRoot |> SorterPoolMemberDto2.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(spmRoot, spmRootBack)

        let spmInit = SorterPoolMember.toInitiate (fun s->initSorter) spmRoot (GenerationNumber.fromInt 2)
        let dtoInit = spmInit |> SorterPoolMemberDto2.toDto
        let spmInitBack = dtoInit |> SorterPoolMemberDto2.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(spmInit, spmInitBack)

        let swUses = SwitchUses.create (SwitchCount.fromInt 22) (Array.init 22 (fun i->i)) |> Result.ExtractOrThrow
        let testResults = 
            {
               SorterTestResults.successfulSortCount = SortableCount.fromInt 11;
               SorterTestResults.switchUses = swUses;
               SorterTestResults.usedStageCount = StageCount.fromInt 33;
               SorterTestResults.usedSwitchCount = SwitchCount.fromInt 44;
            }

        let spmMeas = SorterPoolMember.toMeasured spmInit (fun s->testResults) |> Result.ExtractOrThrow
        let dtoMeas = spmMeas |> SorterPoolMemberDto2.toDto
        let spmMeasBack = dtoMeas |> SorterPoolMemberDto2.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(spmMeas, spmMeasBack)

        let ff2 = FitnessFunc.standardStage
       // let spmEval = SorterPoolMemberF.toEvaluated spmMeas (FitnessFunc.standardStage 1.0) (GenerationNumber.fromInt 3)
        let spmEval = SorterPoolMember.toEvaluated  
                                spmMeas 
                                ff2 
                                (FitnessFuncParam.NoParam)
                                |> Result.ExtractOrThrow

        let dtoEval = spmEval |> SorterPoolMemberDto2.toDto
        let spmEvalBack = dtoEval |> SorterPoolMemberDto2.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(spmEval, spmEvalBack)

        let archiver = fun spm -> spm |> ignore
        let spmArchived = SorterPoolMember.toArchived spmEval |> Result.ExtractOrThrow
        let dtoArchived = spmArchived |> SorterPoolMemberDto2.toDto
        let spmArchivedBack = dtoArchived |> SorterPoolMemberDto2.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(spmArchived, spmArchivedBack)


    [<TestMethod>]
     member this.SorterPoolRunParamsDto() =

        let rngGenkvps = [| ("key0", RngGen.createLcg 123); ("key1", RngGen.createLcg 23); ("key3", RngGen.createLcg 3); |]
        let rngGens = rngGenkvps |>Map.ofArray
        let rngGensDto = rngGens |> Map.map(fun k v -> v |> RngGenDto.toDto)

        let rngGensBack = rngGensDto |> Map.toList 
                            |> List.map(fun (k,v) ->
                                v |> RngGenDto.fromDto |> Result.map(fun r -> (k,r)))
                            |> Result.sequence
                            |> Result.map(fun l -> Map.ofList l)
                            |> Result.ExtractOrThrow

        Assert.AreEqual(rngGens,rngGensBack)