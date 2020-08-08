namespace Archie.Base
open Microsoft.FSharp.Collections
open SorterParts
open System


module SorterOps =

    let SortTR (sorterDef:Sorter) (mindex:int) (maxdex:int) 
               (switchTracker:SwitchTracker) (testCases:SortableSet) 
               (index:int) =
        let weights = (SwitchTracker.weights switchTracker)
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
         let switchTracker = SwitchTracker.create sorterDef.switchCount
         let tcCopy = (SortableSet.copy testCases)
         let mutable i=0
         let mutable success = true
         while (i < testCases.baseArray.Length) do
                  success <- (SortTR sorterDef 0 switchCount switchTracker tcCopy i) &&
                              success
                  i <- i + (Degree.value sorterDef.degree)
         switchTracker, success


    let SortTB (sorterDef:Sorter) (mindex:int) (maxdex:int) 
               (switchUses:SwitchTracker) (lastSwitch:SwitchTracker) 
               (testCases:SortableSet) (index:int) =
        let useWeights = (SwitchTracker.weights switchUses)
        let lastWeights = (SwitchTracker.weights lastSwitch)
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
        lastWeights.[i-1] <- lastWeights.[i-1] + 1


    let SortAllTB (sorterDef:Sorter) (testCases:SortableSet) =
         let switchCount = (SwitchCount.value sorterDef.switchCount)
         let switchUseTracker = SwitchTracker.create sorterDef.switchCount
         let lastSwitchTracker = SwitchTracker.create sorterDef.switchCount
         let tcCopy = (SortableSet.copy testCases)
         let mutable i=0
         while (i < testCases.baseArray.Length) do
                  SortTB sorterDef 0 switchCount switchUseTracker lastSwitchTracker tcCopy i
                  i <- i + (Degree.value sorterDef.degree)
         switchUseTracker, lastSwitchTracker

