namespace Archie.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base



[<TestClass>]
type CommonTypesFixture () =

    [<TestMethod>]
    member this.TestParseRandGenMode() =
        let rgmDto = {RndSorterGenDto.cat="Switch"; args= "11"}
        let res = RndSorterGenDto.fromDto rgmDto |> Result.ExtractOrThrow
        
        let sc = (SwitchCount.create "" 12) |> Result.ExtractOrThrow
        let mm = RndSorterGen.Switch sc
        let mmDto = RndSorterGenDto.toDto mm
        let mmR = RndSorterGenDto.fromDto mmDto |> Result.ExtractOrThrow
        Assert.AreEqual(1, 1)


    [<TestMethod>]
    member this.PrintArray() =
        let st = SorterParts.SwitchUses.create ((SwitchCount.create "" 1000) |> Result.ExtractOrThrow)
        let wts = SorterParts.SwitchUses.getWeights st
        let ss = sprintf "%A" wts |> (fun s->s.Replace("\n ", ""))
        Assert.AreEqual(1, 1)


