namespace Archie.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Archie.Base.SortersFromData
open System

[<TestClass>]
type RunParamsFixture () =

    [<TestMethod>]
    member this.IndexedRandomData() =
       let rg = RngGen.createLcg 123
       let seeds = RandoCollections.IndexedSeedGen rg
                   |> Seq.take(10) |> Seq.toArray

       let guids = RandoCollections.IndexedGuidGen rg None
                    |> Seq.take(10) |> Seq.toArray

       Assert.IsTrue(seeds.Length = 10)
       Assert.IsTrue(guids.Length = 10)


    [<TestMethod>]
    member this.SorterTrimLength() =
      let dd = PoolUpdateParams.MutationTypes 
                    |> Seq.take 10
                    |> Seq.toArray
      Assert.IsTrue(true)

