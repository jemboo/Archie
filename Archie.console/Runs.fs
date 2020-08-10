namespace Archie.console
open Archie.Base
open System
open Archie.Base.SorterParts

module Runs = 
    let mutable lineNum = 0
    let printTitle a =
        lineNum <- lineNum + 1
        Console.WriteLine(sprintf "%d %s" lineNum a)

    let printHeader a =
        lineNum <- lineNum + 1
        Console.WriteLine(sprintf "%d %s" lineNum a)

    let printResult a =
        lineNum <- lineNum + 1
        Console.WriteLine(sprintf "%d %s" lineNum a)



    let DoStep rngType seed degree switchCount sorterCount randGenMode =
        let randy = Rando.RandoFromSeed rngType seed
        Threading.Thread.Sleep 100
        printTitle (sprintf "RndGen:%A\tSeed:%u\tSorters:%d\t%d\t%d\t%A" 
                             rngType seed 
                             (SorterCount.value sorterCount) 
                             (Degree.value degree) 
                             (SwitchCount.value switchCount) 
                             randGenMode)
        printHeader (sprintf "Degree\tRandGenMode\tSwitches\tPassing")
        for i=0 to 3 do
            printResult (sprintf "%A\t%u\t%d\t%d\t%d\t%A\t%d" 
                rngType seed (Degree.value degree) 
                (SwitchCount.value switchCount) 
                (SorterCount.value sorterCount) 
                randGenMode randy.NextUInt)


    //let RunSampler =

    //    let seed = 123
    //    let degree = Degree.create "" 16 |> Result.ExtractOrThrow
    //    let switchCount = SwitchCount.create "" 1000 |> Result.ExtractOrThrow
    //    let sorterCount = SorterCount.create "" 100 |> Result.ExtractOrThrow
    //    let rndType = RngType.Lcg
    //    let seedGen = Rando.RandoFromSeed rndType seed
    //    let randGenMode =  RandSorterGenerationMode.MakeSwitchCount 1600
    //    let mutable step = 0
    //    while true do
    //        //None
    //        DoStep rndType seedGen.NextUInt31 degree switchCount sorterCount randGenMode



    let RunSampler2 =
        printfn "Starting RunSampler2"
        let seedv = 29977
        let seed = RandomSeed.create "" seedv |> Result.ExtractOrThrow
        let degree = Degree.create "" 16 |> Result.ExtractOrThrow
        let switchCount = SwitchCount.create "" 4000 |> Result.ExtractOrThrow
        let sorterCount = SorterCount.create "" 2500 |> Result.ExtractOrThrow
        let rndType = RngType.Lcg
        let randoLcg = new RandomLcg(seed) :> IRando
        let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow
        let sorterSetRnd = SorterSet.createRandom degree switchCount sorterCount randoLcg
        printfn "Degree: %i" (Degree.value degree)
        let res = SortingRun.RunSorterSetOnSortableSetTB sortableSet sorterSetRnd true
                  
        let lastIndexes = res |> Seq.map(snd) |> (SwitchTracker.LastUsedIndexes switchCount)
        printfn "%s" (Utils.printIntArray (SwitchTracker.getWeights lastIndexes))
        6