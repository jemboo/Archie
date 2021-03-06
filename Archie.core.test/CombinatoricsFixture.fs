﻿namespace Archie.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base

[<TestClass>]
type CombinatoricsFixture () =

    [<TestMethod>]
    member this.TestFisherYatesShuffleFromSeed() =
      let starting = [|2; 4; 6; 8; 10; 12; 14 |]
      let rnd = Rando.LcgFromSeed 424 
      let seededFY = Combinatorics.fisherYatesShuffle rnd
      let actual =  seededFY [|2; 4; 6; 8; 10; 12; 14 |] |> Seq.toArray
      Assert.AreEqual(starting.Length, actual.Length)


    [<TestMethod>]
    member this.MakeRandomMonoTwoCycle() =
      let degree = Degree.create "" 5 |> Result.ExtractOrThrow
      let rnd = Rando.LcgFromSeed 424 
      let ts = seq {0 .. 10} 
                    |> Seq.map(fun _ -> Combinatorics.makeRandomMonoTwoCycle degree rnd)
                    |> Seq.toArray
      Assert.IsTrue (ts.Length = 11)


    [<TestMethod>]
    member this.MakeTwoCycle() =
      let degree = Degree.create "" 7 |> Result.ExtractOrThrow
      let rnd = Rando.LcgFromSeed 4242 
      let ts = seq {0 .. 1000} 
                    |> Seq.map(fun _ -> Combinatorics.drawTwoWithoutRep degree rnd)
                    |> Seq.groupBy(id)
                    |> Seq.map(fun t-> (fst t), (snd t)|>Seq.length)
                    |> Seq.toArray
      Assert.IsTrue (ts.Length = 21)


    [<TestMethod>]
    member this.MakeAllMonoTwoCycles() =
      let degree = Degree.create "" 5 |> Result.ExtractOrThrow
      let dd = Combinatorics.makeAllMonoTwoCycles degree |> Seq.toArray
      Assert.IsTrue (dd.Length = 10)
      

    [<TestMethod>]
    member this.TestIntArray01_To_Int() =
      let len = 6
      let expectedArray = [|1; 0; 1; 0; 1; 0|]
      let converted = ZeroOneSequence.FromInteger len 21
      Assert.IsTrue ((expectedArray = converted))


    [<TestMethod>]
    member this.TestCompareArrays() =
      let firstArray = [|2; 4; 6; 8; 10; 12; 14 |]
      let shortArray = [|2; 4; 6; 8; 10; 12; |]
      let otherArray = [|2; 4; 6; 8; 11; 12; 14 |]
      Assert.IsTrue ((firstArray = firstArray))
      Assert.IsFalse ((firstArray = shortArray))
      Assert.IsFalse ((firstArray = otherArray))
 

    [<TestMethod>]
    member this.TestComposeMapIntArraysOnIdentity() =
        let degree = Degree.create "" 6 |> Result.ExtractOrThrow
        let orig = [|5; 4; 3; 2; 1; 0 |]
        let prodR = Combinatorics.composeMapIntArrays orig ((Permutation.identity degree) |> Permutation.arrayValues)
        Assert.IsTrue ((orig = prodR))
        let prodL = Combinatorics.composeMapIntArrays ((Permutation.identity degree) |> Permutation.arrayValues) orig
        Assert.IsTrue ((orig = prodL))
    
    
    [<TestMethod>]
    member this.TestIsSorted() =
        Assert.IsFalse (Combinatorics.isSorted [|0; 1; 1; 0; 1; 0|])
        Assert.IsTrue (Combinatorics.isSorted [|0; 0; 0; 0; 1; 1|])


    [<TestMethod>]
    member this.TestIsSortedOffset() =
        Assert.IsFalse (Combinatorics.isSortedOffset [|0; 1; 1; 0; 1; 0; 1; 1 |] 1 5)
        Assert.IsTrue (Combinatorics.isSortedOffset [|0; 0; 0; 0; 1; 1; 0; 0|] 1 5)


    [<TestMethod>]
    member this.TestEntropy() =
        let res1 = Combinatorics.entropyBits [|1; 1 |]
        Assert.AreEqual (res1, 1.0)
        let res2 = Combinatorics.entropyBits [|1; 1; 1; 1; 0|]
        Assert.AreEqual (res2, 2.0)

    //[<TestMethod>]
    //member this.TestSortIntArray() =
    //    let length = 29
    //    let rnd = Rando.LcgFromSeed 424
    //    let stage = Combinatorics.MakeRandomFullTwoCycleIntArray rnd length
    //    let sortable = Combinatorics.RandomIntPermutations rnd length 1 |> Seq.item 0
    //    let counter = Array.init length (fun i -> 0)

    //    let sortableResult = Combinatorics.SortCopyOfIntArray sortable stage counter
        
    //    let sortableScore = Combinatorics.UnsortednessSquared sortable
    //    let sortableResultScore = Combinatorics.UnsortednessSquared sortableResult

    //    Assert.IsTrue (sortableScore >  sortableResultScore)

    [<TestMethod>]
    member this.InverseMapArray() =
        let degree = Degree.create "" 8 |> Result.ExtractOrThrow
        let randy = Rando.LcgFromSeed 123
        let mutable i = 0
        while i<100 do
            let bloke = Combinatorics.randomPermutation randy (Degree.value degree)
            let inv = Combinatorics.inverseMapArray bloke
            let prod = Combinatorics.composeMapIntArrays bloke inv
            Assert.IsTrue((prod = (Combinatorics.identity (Degree.value degree))))
            i <- i+1

    [<TestMethod>]
    member this.TestConjugateIntArrays() =
        let degree = Degree.create "" 8 |> Result.ExtractOrThrow
        let randy = Rando.LcgFromSeed 123
        let mutable i = 0
        while i<100 do
            let conjer = Combinatorics.randomPermutation randy (Degree.value degree)
            let tc1 = Combinatorics.randomPermutation randy (Degree.value degree)
            let conj1 = Combinatorics.conjugateIntArrays tc1 conjer
            let tc2 = Combinatorics.randomPermutation randy (Degree.value degree)
            let conj2 = Combinatorics.conjugateIntArrays tc2 conjer
            let prod = Combinatorics.composeMapIntArrays tc1 tc2
            let conjprod = Combinatorics.conjugateIntArrays prod conjer
            let prodconj = Combinatorics.composeMapIntArrays conj1 conj2
            Assert.IsTrue((conjprod=prodconj))
            i <- i+1


    [<TestMethod>]
    member this.TestConjugateIntArrays2() =
        let degree = Degree.create "" 8 |> Result.ExtractOrThrow
        let randy = Rando.LcgFromSeed 123
        let mutable i = 0
        while i<100 do
            let tc = Combinatorics.makeRandomFullTwoCycleIntArray randy (Degree.value degree)
            let conjer = Combinatorics.randomPermutation randy (Degree.value degree)
            let conj = Combinatorics.conjugateIntArrays tc conjer
            Assert.IsTrue(Combinatorics.isTwoCycle conj)
            i <- i+1


    [<TestMethod>]
    member this.TestSorted_0_1_Sequence() =
        let blockLen = 10
        let block = IntBits.Sorted_O_1_Sequence blockLen 7 |> Seq.toArray
        Assert.IsTrue (block.Length = blockLen)


    [<TestMethod>]
    member this.makeRandomTwoCycleIntArray() =
        let randy = Rando.LcgFromSeed 123
        let arraySize = 16
        let cycleCount = 2
        let block = Combinatorics.makeRandomTwoCycleIntArray randy arraySize cycleCount
        Assert.IsTrue (Combinatorics.isTwoCycle block)
        let cycleCount = 8
        let block2 = Combinatorics.makeRandomTwoCycleIntArray randy arraySize cycleCount
        Assert.IsTrue (Combinatorics.isTwoCycle block2)


    [<TestMethod>]
    member this.TestSorted_0_1_Sequences() =
        let blockLen = 10
        let block = IntBits.Sorted_0_1_Sequences blockLen
        Assert.IsTrue (block.Length = blockLen + 1)


    [<TestMethod>]
    member this.TestBreakIntoSegments() =
        let testArray = [|1; 2; 3; 4; 5; 6; 7; 8; 9|] 
        let testBreaks = [|0; 2; 5; 9|] 
        let yak = Combinatorics.breakArrayIntoSegments testArray testBreaks
        Assert.AreEqual (yak.Length, 3)


    [<TestMethod>]
    member this.drawFromWeightedDistribution() =
        let testArray = [|2; 3; 4; 5; 6; 7; 8; 9; 10|] 
        let rndy = Rando.LcgFromSeed 44
        let mutable log = []
        let LogRes s =
            log <- s::log
        
        let fitnessFunc (w:int) =
            (SorterFitness.create "" (1.0 / (float w)))
                |> Result.ExtractOrThrow
        testArray |>
        Combinatorics.drawFromWeightedDistribution fitnessFunc rndy
            |> Seq.take 100000 |> Seq.toArray
            |> Array.groupBy(id)
            |> Array.iter(fun b -> LogRes (sprintf "%d %d" (fst b) (snd b).Length))

        Assert.IsTrue (log.Length > 1)


    [<TestMethod>]
    member this.makeHull() =
         let seqX = Seq.initInfinite(fun i-> sprintf "x%d" i)
         let seqY = Seq.initInfinite(fun i-> sprintf "y%d" i)
         let h1 = Combinatorics.makeHull seqX seqY 1 2
                  |> Seq.toArray
         Assert.IsTrue (true)