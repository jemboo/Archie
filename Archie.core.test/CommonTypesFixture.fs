namespace Archie.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base



[<TestClass>]
type CommonTypesFixture() =

    [<TestMethod>]
    member this.TestParseRandGenMode() =
        let rgmDto = {SorterLengthDto.wOrT = "Switch"; value = 11}
        let res = SorterLengthDto.fromDto rgmDto |> Result.ExtractOrThrow
        
        let sc = (SwitchCount.create "" 12) |> Result.ExtractOrThrow
        let mm = SorterLength.Switch sc
        let mmDto = SorterLengthDto.toDto mm
        let mmR = SorterLengthDto.fromDto mmDto |> Result.ExtractOrThrow
        Assert.AreEqual(1, 1)


    [<TestMethod>]
    member this.PrintArray() =
        let st = SwitchUses.create ((SwitchCount.create "" 1000) |> Result.ExtractOrThrow)
        let wts = SwitchUses.getWeights st
        let ss = sprintf "%A" wts |> (fun s->s.Replace("\n ", ""))
        Assert.AreEqual(1, 1)


