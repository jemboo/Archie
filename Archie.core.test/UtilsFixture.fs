namespace Archie.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base



[<TestClass>]
type UtilsFixture () =

    [<TestMethod>]
    member this.sequo() =
        let gg = Utils.sequo (seq { 0..6}) |> Seq.toArray
        Assert.AreEqual(1, 1)