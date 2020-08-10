namespace Archie.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base



[<TestClass>]
type CommonTypesFixture () =

    [<TestMethod>]
    member this.TestParseRandGenMode() =
        let rgmDto = " Switch 11"
        let res = RandSorterGenerationMode.fromDto rgmDto |> Result.ExtractOrThrow
        
        let sc = (SwitchCount.create "" 12) |> Result.ExtractOrThrow
        let mm = RandSorterGenerationMode.Switch sc
        let mmDto = RandSorterGenerationMode.toDto mm
        let mmR = RandSorterGenerationMode.fromDto mmDto |> Result.ExtractOrThrow
        Assert.AreEqual(1, 1)


    [<TestMethod>]
    member this.PrintArray() =
        let st = SorterParts.SwitchTracker.create ((SwitchCount.create "" 1000) |> Result.ExtractOrThrow)
        let wts = SorterParts.SwitchTracker.getWeights st
        let ss = sprintf "%A" wts |> (fun s->s.Replace("\n ", ""))
        Assert.AreEqual(1, 1)


