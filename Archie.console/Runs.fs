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


    let RunSampler =
        let seedv = 29977
        let seed = RandomSeed.create "" seedv |> Result.ExtractOrThrow
        let degree = Degree.create "" 16 |> Result.ExtractOrThrow
        let switchCount = SwitchCount.create "" 1000 |> Result.ExtractOrThrow
        let sorterCount = SorterCount.create "" 250 |> Result.ExtractOrThrow
        let rndType = RngType.Lcg
        let log = [sprintf "Degree: %i Seed: %i" (Degree.value degree) seedv]

        let randoLcg = new RandomLcg(seed) :> IRando
        let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow
        let sorterSetRnd = SorterSet.createRandom degree switchCount sorterCount randoLcg
        let res = SortingRun.RunSorterSetOnSortableSetTB sortableSet sorterSetRnd true
        let lastIndexes = res |> (SwitchUses.LastUsedIndexes switchCount)
        let log = (Utils.printIntArray (SwitchUses.getWeights lastIndexes))::log
        log


    let RunSampler2 =
        let seedv = 29977
        let seedw = 277
        let rndSeedv = RandomSeed.create "" seedv |> Result.ExtractOrThrow
        let rndSeedw = RandomSeed.create "" seedw |> Result.ExtractOrThrow
        let degree = Degree.create "" 16 |> Result.ExtractOrThrow
        let switchCount = SwitchCount.create "" 1000 |> Result.ExtractOrThrow
        let sorterCount = SorterCount.create "" 250 |> Result.ExtractOrThrow
        let rndType = RngType.Lcg
        let log = [sprintf "Degree: %i Seed: %i" (Degree.value degree) seedv]

        let randoLcgV = new RandomLcg(rndSeedv) :> IRando
        let randoLcgW = new RandomLcg(rndSeedw) :> IRando

        let sortableSet = SortableSet.allBinary degree |> Result.ExtractOrThrow
        let sorterSetRnd = (SorterSet.createRandom degree switchCount sorterCount randoLcgV)
                            |> (SorterSetE.fromSorterSet randoLcgV randoLcgW)
        let res = SortingRun.RunSorterSetOnSortableSetTBE sortableSet sorterSetRnd true
        //let useCounts = res |> Array.map(fun d->(Dependent.character d)) 
        //                    |> Array.map(SwitchUses.UseCount)
        let useCounts = res |> Array.map(fun d-> Dependent.map SwitchUses.UseCount d)
        let log = (Utils.printArrayf Dependent.format useCounts)::log
        log
       