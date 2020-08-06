namespace Archie.Base
open Microsoft.FSharp.Collections
open SorterParts
open System

module SorterOps3 =

    let Sort (sorterDef:Sorter) (mindex:int) (maxdex:int) 
             (testCases:SortableSet3) (index:int) =
        let span = new Span<int>(testCases.baseArray)
        let slice = span.Slice(index, Degree.value(testCases.degree))
        for i=mindex to maxdex-1 do
            let switch = sorterDef.switches.[i]
            let lv = slice.[switch.low]
            let hv = slice.[switch.hi]
            if(lv > hv) then
                slice.[switch.hi] <- lv
                slice.[switch.low] <- hv


    let SortAll (sorterDef:Sorter) (testCases:SortableSet3) =
           let switchCount = (SwitchCount.value sorterDef.switchCount)
           let tcCopy = (SortableSet3.copy testCases)
           let mutable i=0
           while (i < testCases.baseArray.Length) do
                Sort sorterDef 0 switchCount tcCopy i
                i <- i + (Degree.value sorterDef.degree)


    let SortT (sorterDef:Sorter) (mindex:int) (maxdex:int) 
              (switchTracker:SwitchTracker) (testCases:SortableSet3) (index:int) =
        let span = new Span<int>(testCases.baseArray)
        let slice = span.Slice(index, Degree.value(testCases.degree))
        let weights = (SwitchTracker.weights switchTracker)
        for i=mindex to maxdex-1 do
            let switch = sorterDef.switches.[i]
            let lv = slice.[switch.low]
            let hv = slice.[switch.hi]
            if(lv > hv) then
                slice.[switch.hi] <- lv
                slice.[switch.low] <- hv
                weights.[i] <- weights.[i] + 1


    let SortAllT (sorterDef:Sorter) (testCases:SortableSet3) =
         let switchCount = (SwitchCount.value sorterDef.switchCount)
         let switchTracker = SwitchTracker.create sorterDef.switchCount
         let tcCopy = (SortableSet3.copy testCases)
         let mutable i=0
         while (i < testCases.baseArray.Length) do
                  SortT sorterDef 0 switchCount switchTracker tcCopy i
                  i <- i + (Degree.value sorterDef.degree)
         switchTracker


    let SortTR (sorterDef:Sorter) (mindex:int) (maxdex:int) 
               (switchTracker:SwitchTracker) (testCases:SortableSet3) (index:int) =
        let span = new Span<int>(testCases.baseArray)
        let slice = span.Slice(index, Degree.value(testCases.degree))
        let weights = (SwitchTracker.weights switchTracker)
        for i=mindex to maxdex-1 do
            let switch = sorterDef.switches.[i]
            let lv = slice.[switch.low]
            let hv = slice.[switch.hi]
            if(lv > hv) then
                slice.[switch.hi] <- lv
                slice.[switch.low] <- hv
                weights.[i] <- weights.[i] + 1
        Combinatorics.SpanIsSorted slice


    let SortAllTR (sorterDef:Sorter) (testCases:SortableSet3) =
         let switchCount = (SwitchCount.value sorterDef.switchCount)
         let switchTracker = SwitchTracker.create sorterDef.switchCount
         let tcCopy = (SortableSet3.copy testCases)
         let mutable i=0
         let mutable success = true
         while (i < testCases.baseArray.Length) do
                  success <- (SortTR sorterDef 0 switchCount switchTracker tcCopy i) &&
                              success
                  i <- i + (Degree.value sorterDef.degree)
         switchTracker, success


    let SortTB (sorterDef:Sorter) (mindex:int) (maxdex:int) 
               (switchUses:SwitchTracker) (lastSwitch:SwitchTracker) 
               (testCases:SortableSet3) (index:int) =
        let span = new Span<int>(testCases.baseArray)
        let slice = span.Slice(index, Degree.value(testCases.degree))
        let useWeights = (SwitchTracker.weights switchUses)
        let lastWeights = (SwitchTracker.weights lastSwitch)
        let mutable looP = true
        let mutable i = mindex

        while ((i < maxdex) && looP) do
            let switch = sorterDef.switches.[i]
            let lv = slice.[switch.low]
            let hv = slice.[switch.hi]
            if(lv > hv) then
                slice.[switch.hi] <- lv
                slice.[switch.low] <- hv
                useWeights.[i] <- useWeights.[i] + 1
                looP <- not (Combinatorics.SpanIsSorted slice) 
            i <- i+1
        lastWeights.[i-1] <- lastWeights.[i-1] + 1


    let SortAllTB (sorterDef:Sorter) (testCases:SortableSet3) =
         let switchCount = (SwitchCount.value sorterDef.switchCount)
         let switchUseTracker = SwitchTracker.create sorterDef.switchCount
         let lastSwitchTracker = SwitchTracker.create sorterDef.switchCount
         let tcCopy = (SortableSet3.copy testCases)
         let mutable i=0
         while (i < testCases.baseArray.Length) do
                  SortTB sorterDef 0 switchCount switchUseTracker lastSwitchTracker tcCopy i
                  i <- i + (Degree.value sorterDef.degree)
         switchUseTracker, lastSwitchTracker


module SorterOps2 =

    let Sort (sorterDef:Sorter) (mindex:int) (maxdex:int) 
             (testCases:SortableSet) (index:int) =
        let span = new Span<int>(testCases.baseArray)
        let slice = span.Slice(index, Degree.value(testCases.degree))
        for i=mindex to maxdex-1 do
            let switch = sorterDef.switches.[i]
            let lv = slice.[switch.low]
            let hv = slice.[switch.hi]
            if(lv > hv) then
                slice.[switch.hi] <- lv
                slice.[switch.low] <- hv


    let SortAll (sorterDef:Sorter) (testCases:SortableSet) =
           let switchCount = (SwitchCount.value sorterDef.switchCount)
           let tcCopy = (SortableSet.copy testCases)
           let mutable i=0
           while (i < testCases.baseArray.Length) do
                Sort sorterDef 0 switchCount tcCopy i
                i <- i + (Degree.value sorterDef.degree)


    let SortT (sorterDef:Sorter) (mindex:int) (maxdex:int) 
              (switchTracker:SwitchTracker) (testCases:SortableSet) 
              (index:int) =
        let span = new Span<int>(testCases.baseArray)
        let slice = span.Slice(index, Degree.value(testCases.degree))
        let weights = (SwitchTracker.weights switchTracker)
        for i=mindex to maxdex-1 do
            let switch = sorterDef.switches.[i]
            let lv = slice.[switch.low]
            let hv = slice.[switch.hi]
            if(lv > hv) then
                slice.[switch.hi] <- lv
                slice.[switch.low] <- hv
                weights.[i] <- weights.[i] + 1


    let SortAllT (sorterDef:Sorter) (testCases:SortableSet) =
         let switchCount = (SwitchCount.value sorterDef.switchCount)
         let switchTracker = SwitchTracker.create sorterDef.switchCount
         let tcCopy = (SortableSet.copy testCases)
         let mutable i=0
         while (i < testCases.baseArray.Length) do
                  SortT sorterDef 0 switchCount switchTracker tcCopy i
                  i <- i + (Degree.value sorterDef.degree)
         switchTracker


    let SortTR (sorterDef:Sorter) (mindex:int) (maxdex:int) 
               (switchTracker:SwitchTracker) (testCases:SortableSet3) 
               (index:int) =
        let span = new Span<int>(testCases.baseArray)
        let slice = span.Slice(index, Degree.value(testCases.degree))
        let weights = (SwitchTracker.weights switchTracker)
        for i=mindex to maxdex-1 do
            let switch = sorterDef.switches.[i]
            let lv = slice.[switch.low]
            let hv = slice.[switch.hi]
            if(lv > hv) then
                slice.[switch.hi] <- lv
                slice.[switch.low] <- hv
                weights.[i] <- weights.[i] + 1
        Combinatorics.SpanIsSorted slice


    let SortAllTR (sorterDef:Sorter) (testCases:SortableSet3) =
         let switchCount = (SwitchCount.value sorterDef.switchCount)
         let switchTracker = SwitchTracker.create sorterDef.switchCount
         let tcCopy = (SortableSet3.copy testCases)
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
        let span = new Span<int>(testCases.baseArray)
        let slice = span.Slice(index, Degree.value(testCases.degree))
        let useWeights = (SwitchTracker.weights switchUses)
        let lastWeights = (SwitchTracker.weights lastSwitch)
        let mutable looP = true
        let mutable i = mindex

        while ((i < maxdex) && looP) do
            let switch = sorterDef.switches.[i]
            let lv = slice.[switch.low]
            let hv = slice.[switch.hi]
            if(lv > hv) then
                slice.[switch.hi] <- lv
                slice.[switch.low] <- hv
                useWeights.[i] <- useWeights.[i] + 1
                looP <- not (Combinatorics.SpanIsSorted slice) 
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

