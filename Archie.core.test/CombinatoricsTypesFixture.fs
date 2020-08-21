namespace Archie.core.test
open Archie.Base

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type CombinatoricsTypesFixture () =

    [<TestMethod>]
    member this.TestIdentityPermutation() =
      let expectedLen = 9
      let expectedSum = ( expectedLen * (expectedLen - 1)) / 2
      let permutes = Permutation.Identity  (Degree.create "" expectedLen |> Result.ExtractOrThrow)
      Assert.AreEqual(expectedLen, permutes |> Permutation.arrayValues |> Array.length)
      Assert.AreEqual(expectedSum, permutes |> Permutation.arrayValues |> Array.sum)

    [<TestMethod>]
    member this.TestInversePermutation() =
       let degree = Degree.create "" 6 |> Result.ExtractOrThrow
       let rnd = Rando.LcgFromSeed 424
       let perm = Permutation.CreateRandom degree rnd
       let inv = Permutation.Inverse perm |> Result.ExtractOrThrow
       let prod = Permutation.Product perm inv |> Result.ExtractOrThrow
       let id = Permutation.Identity degree
       Assert.AreEqual(id, prod)

    [<TestMethod>]
    member this.TestMakeMonoCycle() =
       let degree = Degree.create "" 6 |> Result.ExtractOrThrow
       let rnd = Rando.LcgFromSeed 424
       let tc = TwoCyclePerm.MakeMonoCycle degree 2 3 |> Result.ExtractOrThrow
       let id = TwoCyclePerm.Identity degree
       let tcSq = TwoCyclePerm.Product tc tc |> Result.ExtractOrThrow
       Assert.AreNotEqual(tc, id)
       Assert.AreEqual(id, tcSq)

    [<TestMethod>]
    member this.TestMakeAllMonoCycles() =
       let degree = Degree.create "" 6 |> Result.ExtractOrThrow
       let rnd = Rando.LcgFromSeed 424
       let tcA = TwoCyclePerm.MakeAllMonoCycles degree |> Seq.toArray
       let tcASq = tcA |> Array.map (fun tcp -> TwoCyclePerm.Product tcp tcp 
                                                |> Result.ExtractOrThrow)
       let id = TwoCyclePerm.Identity degree
       Assert.AreEqual(tcA.Length, 15)
       for i in {0 .. tcASq.Length - 1} do
          Assert.AreEqual(tcASq.[i], id)

    [<TestMethod>]
    member this.TestMakeMakeRandomPolyCycle() =
       let degree = Degree.create "" 9 |> Result.ExtractOrThrow
       let rnd = Rando.LcgFromSeed 424
       let id = TwoCyclePerm.Identity degree
       for i in {0 .. 20} do
                let tcp = TwoCyclePerm.MakeRandomFullTwoCycle degree rnd
                let tSq = TwoCyclePerm.Product tcp tcp |> Result.ExtractOrThrow
                Assert.AreEqual(tSq, id)

    [<TestMethod>]
    member this.TestB() =
       let aa = IntBits.AllBinaryTestCasesArray 5
       //let b0 = Array2D.length1 aa

       Assert.IsTrue(true)

       
    [<TestMethod>]
    member this.makeFromTupleSeq() =        
        let rndy = Rando.LcgFromSeed 44
        let degree = Degree.create "" 16 |> Result.ExtractOrThrow
        let stageTupes = SorterParts.Stage.makeStagePackedSwitchSeq degree rndy
                         |> Seq.map(fun s -> (s.low, s.hi))
                         |> Seq.take 8
        let twoCycle = TwoCyclePerm.makeFromTupleSeq degree stageTupes
        let product = twoCycle |> TwoCyclePerm.Product twoCycle |> Result.ExtractOrThrow
        Assert.AreEqual(product, TwoCyclePerm.Identity degree)
        Assert.IsTrue (true)