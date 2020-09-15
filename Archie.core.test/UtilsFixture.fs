namespace Archie.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open System.Linq



[<TestClass>]
type UtilsFixture () =

    [<TestMethod>]
    member this.sequo() =
        let ts = [|1; 2; 3|]
        let reppy = CollectionUtils.IterateCircular 10 ts  |> Seq.toArray
        
        Assert.IsTrue(reppy.Length = 10)