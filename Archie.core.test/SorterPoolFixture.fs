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
    member this.makeBatchOfRunReplicas() =
        let rngSorters = RngGen.createLcg 123
        let rngTypeForSorterIds = RngType.Lcg
        let degree = Degree.fromInt 10
        let sorterLength = SorterLength.to999Sucessful degree SwitchOrStage.Stage
        let switchFreq = (SwitchFrequency.fromFloat 1.0)
        let sorterPoolSize = SorterCount.fromInt 2
        let rngGenMut = RngGen.createLcg 456
        let breederFrac = PoolFraction.fromFloat 0.5
        let winnerFrac = PoolFraction.fromFloat 0.5
        let sorterMutationType = SorterMutationType.Stage (MutationRate.fromFloat 0.1)
        let runLength =  GenerationNumber.fromInt 99
        let poolCount = PoolCount.fromInt 3
        let runCount = RunCount.fromInt 12

        let batch = SorterPoolBatchRunParams.makeBatchOfRunReplicas rngSorters
                        rngTypeForSorterIds degree sorterLength switchFreq
                        sorterPoolSize rngGenMut breederFrac winnerFrac
                        sorterMutationType runLength poolCount runCount

        Assert.IsTrue(true);


    [<TestMethod>]
    member this.MakeRandomSorter() =
        Assert.IsTrue(true);
