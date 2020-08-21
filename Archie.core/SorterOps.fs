namespace Archie.Base
open Microsoft.FSharp.Collections
open SorterParts
open System


module SorterOps =

    let SortTR (sorter:Sorter) (mindex:int) (maxdex:int) 
               (switchTracker:SwitchUses) (testCases:SortableSet) 
               (index:int) =
        let weights = (SwitchUses.getWeights switchTracker)
        let mutable i = mindex
        while (i < maxdex) do
            let switch = sorter.switches.[i]
            let lv = testCases.baseArray.[switch.low + index]
            let hv = testCases.baseArray.[switch.hi + index]
            if(lv > hv) then
                testCases.baseArray.[switch.hi + index] <- lv
                testCases.baseArray.[switch.low + index] <- hv
                weights.[i] <- weights.[i] + 1
            i <- i+1
        Combinatorics.IsSortedOffset testCases.baseArray index (Degree.value(testCases.degree))

    let SortAllTR (sorter:Sorter) (testCases:SortableSet) =
         let switchCount = (SwitchCount.value sorter.switchCount)
         let switchTracker = SwitchUses.create sorter.switchCount
         let tcCopy = (SortableSet.copy testCases)
         let mutable i=0
         let mutable success = true
         while (i < testCases.baseArray.Length) do
                  success <- (SortTR sorter 0 switchCount switchTracker tcCopy i) &&
                              success
                  i <- i + (Degree.value sorter.degree)
         sorter, switchTracker, success

    let SortTB (sorter:Sorter) (mindex:int) (maxdex:int) 
               (switchUses:SwitchUses) (testCases:SortableSet) (index:int) =
        let useWeights = (SwitchUses.getWeights switchUses)
        let mutable looP = true
        let mutable i = mindex
        while ((i < maxdex) && looP) do
            let switch = sorter.switches.[i]
            let lv = testCases.baseArray.[switch.low + index]
            let hv = testCases.baseArray.[switch.hi + index]
            if(lv > hv) then
                testCases.baseArray.[switch.hi + index] <- lv
                testCases.baseArray.[switch.low + index] <- hv
                useWeights.[i] <- useWeights.[i] + 1
                looP <- not (Combinatorics.IsSortedOffset testCases.baseArray 
                                                            index 
                                                            (Degree.value(testCases.degree))) 
            i <- i+1
        Combinatorics.IsSortedOffset testCases.baseArray index (Degree.value(testCases.degree))

    let SortAllTB (sorter:Sorter) (testCases:SortableSet) =
         let switchCount = (SwitchCount.value sorter.switchCount)
         let switchUses = SwitchUses.create sorter.switchCount
         let tcCopy = (SortableSet.copy testCases)
         let mutable i=0
         let mutable success = true
         while (i < testCases.baseArray.Length) do
                  success <- (SortTB sorter 0 switchCount switchUses tcCopy i) &&
                              success
                  i <- i + (Degree.value sorter.degree)
         sorter, switchUses, success
         

    let private SorterSetOnSortableSet (sortableSet:SortableSet) (sorters:Sorter[]) 
                                       (_parallel:bool) sorterOnSortableSet =
        match _parallel with
        | true -> sorters |> Array.Parallel.map(
                        fun s-> sorterOnSortableSet s sortableSet)
        | false -> sorters |> Array.map(
                        fun s-> sorterOnSortableSet s sortableSet)


    let CompleteSort (sortableSet:SortableSet) (sorters:Sorter[])
                     (_parallel:bool) =
        SortAllTR |> SorterSetOnSortableSet sortableSet sorters _parallel


    let StopIfSorted (sortableSet:SortableSet) (sorters:Sorter[])
                     (_parallel:bool) =
        SortAllTB |> SorterSetOnSortableSet sortableSet sorters _parallel
         
