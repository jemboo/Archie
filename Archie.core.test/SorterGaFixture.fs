namespace Archie.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Archie.Base.SorterParts
open Archie.Base.SortersFromData
open System

[<TestClass>]
type SorterFixture () =

    [<TestMethod>]
    member this.IndexedRandomData() =
       let rg = RngGen.createLcg 123
       let seeds = PoolUpdateParams.IndexedSeedGen rg
                   |> Seq.take(10) |> Seq.toArray

       let guids = PoolUpdateParams.IndexedGuidGen rg None
                    |> Seq.take(10) |> Seq.toArray

       Assert.IsTrue(seeds.Length = 10)
       Assert.IsTrue(guids.Length = 10)


    [<TestMethod>]
    member this.SorterTrimLength() =
      let dd = PoolUpdateParams.MutationTypes 
                    |> Seq.take 10
                    |> Seq.toArray
      Assert.IsTrue(true)

