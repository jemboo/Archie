namespace Archie.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Archie.Base.SortersFromData
open System

[<TestClass>]
type GaRunDtoFixture() =

    [<TestMethod>]
    member this.MapWithKeyDupes() =
       let kvps = seq { ("a", 1); ("b", 2); ("c", 3); ("b", 5) } |> Seq.toArray
       let ma = CollectionUtils.tuplesToMap kvps
       let result = match ma with
                       | Ok m -> "success"
                       | Error m -> m
       Assert.IsTrue((result = "key dupicates"))


    [<TestMethod>]
    member this.MapWithoutKeyDupes() =
       let kvps = seq { ("a", 1); ("b", 2); ("c", 3); } |> Seq.toArray
       let ma = CollectionUtils.tuplesToMap kvps
       let result = match ma with
                       | Ok m -> "success"
                       | Error m -> m
       Assert.IsTrue((result = "success"))


    [<TestMethod>]
    member this.Test2() =

      Assert.IsTrue(true)

