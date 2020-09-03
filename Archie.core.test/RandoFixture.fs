namespace Archie.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base

[<TestClass>]
type RandoFixture () =

    [<TestMethod>]
    member this.MakeUint () =
        let seed = RandomSeed.create "" 424 |> Result.ExtractOrThrow
        let randoLcg = new RandomLcg(seed)
        let rv = int (randoLcg.NextUInt % 500u)
        Assert.IsTrue(rv > 0);
        let rv = int (randoLcg.NextUInt % 500u)
        Assert.IsTrue(rv > 0);
        let rv = int (randoLcg.NextUInt % 500u)
        Assert.IsTrue(rv > 0);
        let rv = int (randoLcg.NextUInt % 500u)
        Assert.IsTrue(rv > 0);


    [<TestMethod>]
    member this.TestPermutations () =        
        let degree = 6 |> Degree.create "" |> Result.ExtractOrThrow
        let seed = RandomSeed.create "" 424 |> Result.ExtractOrThrow
        let randoLcg = new RandomLcg(seed)
        let sortableCount = 4

        let perms = Permutation.createRandoms degree randoLcg
                    |> Seq.map(fun i -> Permutation.arrayValues i )
                    |> Seq.take sortableCount

        Assert.IsTrue(true);


    [<TestMethod>]
    member this.RngGenDto() =
        let rngGen = {RngGen.rngType=RngType.Lcg; seed = RandomSeed.create "" 123|>Result.ExtractOrThrow}
        let dto = RngGenDto.toDto rngGen
        let rngGenBack = RngGenDto.fromDto dto |> Result.ExtractOrThrow
        Assert.IsTrue((rngGen=rngGenBack))
        

    [<TestMethod>]
    member this.IndexedRandomData() =
        let rg = RngGen.createLcg 123
        let seeds = RandoCollections.IndexedSeedGen rg
                    |> Seq.take(10) |> Seq.toArray

        let guids = RandoCollections.IndexedGuidGen rg None
                    |> Seq.take(10) |> Seq.toArray

        Assert.IsTrue(seeds.Length = 10)

