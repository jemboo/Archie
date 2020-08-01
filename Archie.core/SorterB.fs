namespace Archie.Base
open Microsoft.FSharp.Collections
open Sorting
open System

module Sorter2 =
   
    let SortWithSlice (sorterDef:SorterDef) (mindex:int) (maxdex:int) (sortable:Sortable2) =
        let span = new Span<int>(sortable.baseArray)
        let slice = span.Slice(sortable.offset, Degree.value(sortable.degree))
        for i=mindex to maxdex-1 do
            let switch = sorterDef.switches.[i]
            let lv = slice.[switch.low]
            let hv = slice.[switch.hi]
            if(lv > hv) then
                slice.[switch.hi] <- lv
                slice.[switch.low] <- hv


    let SortR (sorterDef:SorterDef) (mindex:int) (maxdex:int) 
              (sortReaction:int->unit) (sortable:Sortable2) =
        let offset =sortable.offset
        let baseArray = sortable.baseArray
        for i=mindex to maxdex-1 do
            let switch = sorterDef.switches.[i]
            let lv = baseArray.[switch.low + offset]
            let hv = baseArray.[switch.hi + offset]
            if(lv > hv) then
                baseArray.[switch.hi + offset] <- lv
                baseArray.[switch.low + offset] <- hv
                sortReaction i


    let SortAllAndTrackR (sorterDef:SorterDef) (testCases:Sortable2[]) =
         let switchCount = (SwitchCount.value sorterDef.switchCount)
         let switchTracker = SwitchTracker.create sorterDef.switchCount
         let weights = (SwitchTracker.weights switchTracker)
         let sortReaction = fun i -> weights.[i] <- weights.[i] + 1
         let mutable i=0
         while (i < testCases.Length) do
                  SortR sorterDef 0 switchCount sortReaction testCases.[i]
                  i<-i+1
         switchTracker


    let SortR2 (sorterDef:SorterDef) (mindex:int) (maxdex:int) 
              (sortReaction:int->unit) (sortable:Sortable2) =
        let offset =sortable.offset
        let baseArray = sortable.baseArray
        for i=mindex to maxdex-1 do
            let switch = sorterDef.switches.[i]
            let lv = baseArray.[switch.low + offset]
            let hv = baseArray.[switch.hi + offset]
            if(lv > hv) then
                baseArray.[switch.hi + offset] <- lv
                baseArray.[switch.low + offset] <- hv
                sortReaction i


    let SortAndTrackR2 (sorterDef:SorterDef) (testCases:Sortable2[]) =
         let switchCount = (SwitchCount.value sorterDef.switchCount)
         let switchTracker = SwitchTracker.create sorterDef.switchCount
         let weights = (SwitchTracker.weights switchTracker)
         let sortReaction = fun i -> weights.[i] <- weights.[i] + 1
         let mutable i=0
         while (i < testCases.Length) do
                  SortR2 sorterDef 0 switchCount sortReaction testCases.[i]
                  i<-i+1
         switchTracker


    let SortT (sorterDef:SorterDef) (mindex:int) (maxdex:int) 
              (switchTracker:SwitchTracker) (sortable:Sortable2) =
        let offset =sortable.offset
        let baseArray = sortable.baseArray
        let weights = (SwitchTracker.weights switchTracker)
        for i=mindex to maxdex-1 do
            let switch = sorterDef.switches.[i]
            let lv = baseArray.[switch.low + offset]
            let hv = baseArray.[switch.hi + offset]
            if(lv > hv) then
                baseArray.[switch.hi + offset] <- lv
                baseArray.[switch.low + offset] <- hv
                weights.[i] <- weights.[i] + 1


    let SortAllAndTrackT (sorterDef:SorterDef) (testCases:Sortable2[]) =
         let switchCount = (SwitchCount.value sorterDef.switchCount)
         let switchTracker = SwitchTracker.create sorterDef.switchCount
         let mutable i=0
         while (i < testCases.Length) do
                  SortT sorterDef 0 switchCount switchTracker testCases.[i]
                  i<-i+1
         switchTracker


    let SortTB (sorterDef:SorterDef) (mindex:int) (maxdex:int) 
               (switchUses:SwitchTracker) (lastSwitch:SwitchTracker) 
               (sortable:Sortable2) =
        let offset = sortable.offset
        let baseArray = sortable.baseArray
        let useWeights = (SwitchTracker.weights switchUses)
        let lastWeights = (SwitchTracker.weights lastSwitch)
        let mutable looP = true
        let mutable i = mindex

        while ((i < maxdex) && looP) do
            let switch = sorterDef.switches.[i]
            let lv = baseArray.[switch.low + offset]
            let hv = baseArray.[switch.hi + offset]
            if(lv > hv) then
                baseArray.[switch.hi + offset] <- lv
                baseArray.[switch.low + offset] <- hv
                useWeights.[i] <- useWeights.[i] + 1
                looP <- not (Combinatorics.IsSortedOffset baseArray offset (Degree.value sorterDef.degree)) 
            i <- i+1
        lastWeights.[i-1] <- lastWeights.[i-1] + 1


    let SortAllAndTrackTB (sorterDef:SorterDef) (testCases:Sortable2[]) =
         let switchCount = (SwitchCount.value sorterDef.switchCount)
         let switchUseTracker = SwitchTracker.create sorterDef.switchCount
         let lastSwitchTracker = SwitchTracker.create sorterDef.switchCount
         let mutable i=0
         while (i < testCases.Length) do
                  SortTB sorterDef 0 switchCount switchUseTracker lastSwitchTracker testCases.[i]
                  i<-i+1
         switchUseTracker, lastSwitchTracker


    let Sort (sorterDef:SorterDef) (mindex:int) (maxdex:int) (sortable:Sortable2) =
        let offset =sortable.offset
        let baseArray = sortable.baseArray
        for i=mindex to maxdex-1 do
            let switch = sorterDef.switches.[i]
            let lv = baseArray.[switch.low + offset]
            let hv = baseArray.[switch.hi + offset]
            if(lv > hv) then
                baseArray.[switch.hi + offset] <- lv
                baseArray.[switch.low + offset] <- hv


    let SortAll (sorterDef:SorterDef) (testCases:Sortable2[]) =
           let switchCount = (SwitchCount.value sorterDef.switchCount)
           let mutable i=0
           while (i < testCases.Length) do
                Sort sorterDef 0 switchCount testCases.[i]
                i<-i+1


    let SortAllWithSlice (sorterDef:SorterDef) (testCases:Sortable2[]) =
           let switchCount = (SwitchCount.value sorterDef.switchCount)
           let mutable i=0
           while (i < testCases.Length) do
                SortWithSlice sorterDef 0 switchCount testCases.[i]
                i<-i+1