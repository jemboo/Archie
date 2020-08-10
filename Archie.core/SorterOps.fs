namespace Archie.Base
open Microsoft.FSharp.Collections
open SorterParts
open System


module SorterOps =

    let SortTR (sorterDef:Sorter) (mindex:int) (maxdex:int) 
               (switchTracker:SwitchUses) (testCases:SortableSet) 
               (index:int) =
        let weights = (SwitchUses.getWeights switchTracker)
        let mutable i = mindex
        while (i < maxdex) do
            let switch = sorterDef.switches.[i]
            let lv = testCases.baseArray.[switch.low + index]
            let hv = testCases.baseArray.[switch.hi + index]
            if(lv > hv) then
                testCases.baseArray.[switch.hi + index] <- lv
                testCases.baseArray.[switch.low + index] <- hv
                weights.[i] <- weights.[i] + 1
            i <- i+1
        Combinatorics.IsSortedOffset testCases.baseArray index (Degree.value(testCases.degree))


    let SortAllTR (sorterDef:Sorter) (testCases:SortableSet) =
         let switchCount = (SwitchCount.value sorterDef.switchCount)
         let switchTracker = SwitchUses.create sorterDef.switchCount
         let tcCopy = (SortableSet.copy testCases)
         let mutable i=0
         let mutable success = true
         while (i < testCases.baseArray.Length) do
                  success <- (SortTR sorterDef 0 switchCount switchTracker tcCopy i) &&
                              success
                  i <- i + (Degree.value sorterDef.degree)
         switchTracker, success


    let SortTB (sorterDef:Sorter) (mindex:int) (maxdex:int) 
               (switchUses:SwitchUses) (testCases:SortableSet) (index:int) =
        let useWeights = (SwitchUses.getWeights switchUses)
        let mutable looP = true
        let mutable i = mindex
        while ((i < maxdex) && looP) do
            let switch = sorterDef.switches.[i]
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


    let SortAllTB (sorterDef:Sorter) (testCases:SortableSet) =
         let switchCount = (SwitchCount.value sorterDef.switchCount)
         let switchUseTracker = SwitchUses.create sorterDef.switchCount
         let tcCopy = (SortableSet.copy testCases)
         let mutable i=0
         while (i < testCases.baseArray.Length) do
                  SortTB sorterDef 0 switchCount switchUseTracker tcCopy i
                  i <- i + (Degree.value sorterDef.degree)
         switchUseTracker

