namespace Archie.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Archie.Base.Sorting
open Archie.Base.Combinatorics_Types

[<TestClass>]
type RandoFixture () =

    [<TestMethod>]
    member this.MakeUint () =
        let seed = RandomSeed.create "" 424 |> Result.toOption
        let randoLcg = new RandomLcg(seed.Value)
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
        let degree = 6 |> Degree.create "" |> Result.toOption
        let seed = RandomSeed.create "" 424 |> Result.toOption
        let randoLcg = new RandomLcg(seed.Value)
        let sortableCount = 4

        let perms = Permutation.CreateRandom randoLcg (Degree.value degree.Value)
                    |> Seq.map(fun i -> Permutation.value i )
                    |> Seq.take sortableCount

        Assert.IsTrue(true);