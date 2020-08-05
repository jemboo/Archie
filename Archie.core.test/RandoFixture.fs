namespace Archie.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Archie.Base.SorterOps

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

        let perms = Permutation.CreateRandoms degree randoLcg
                    |> Seq.map(fun i -> Permutation.arrayValues i )
                    |> Seq.take sortableCount

        Assert.IsTrue(true);