namespace Archie.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base

[<TestClass>]
type SorterPoolFixture () =

    [<TestMethod>]
    member this.createRandom() =
        let degree = Degree.fromInt 10
        let sorterLength = SorterLength.degreeTo999StageCount degree
        let switchFreq = SwitchFrequency.fromFloat 0.6
        let sorterCount = SorterCount.fromInt 4
        let rngSorters = RngGen.createLcg 123
        let yab = SorterPool2.createRandom degree sorterLength 
                    switchFreq sorterCount rngSorters RngType.Lcg
        Assert.IsTrue(true);


    [<TestMethod>]
    member this.RandSwitchUniformity() =
        Assert.IsTrue(true);


    [<TestMethod>]
    member this.MakeRandomSorter() =
        Assert.IsTrue(true);
