namespace Archie.Base
open Microsoft.FSharp.Collections
open Sorting
open System

module Sorter =

    let Sort (sorterDef:SorterDef) (mindex:int) (maxdex:int) (sortable:Sortable) =
        let span = new Span<int>(sortable.baseArray)
        let slice = span.Slice(sortable.offset, Degree.value(sortable.degree))
        for i=mindex to maxdex-1 do
            let switch = sorterDef.switches.[i]
            let lv = slice.[switch.low]
            let hv = slice.[switch.hi]
            if(lv > hv) then
                slice.[switch.hi] <- lv
                slice.[switch.low] <- hv


    let SortAll (sorterDef:SorterDef) (testCases:Sortable[]) =
           let switchCount = (SwitchCount.value sorterDef.switchCount)
           let mutable i=0
           while (i < testCases.Length) do
                Sort sorterDef 0 switchCount testCases.[i]
                i<-i+1


    let SortT (sorterDef:SorterDef) (mindex:int) (maxdex:int) 
              (switchTracker:SwitchTracker) (sortable:Sortable) =
        let span = new Span<int>(sortable.baseArray)
        let slice = span.Slice(sortable.offset, Degree.value(sortable.degree))
        let weights = (SwitchTracker.weights switchTracker)
        for i=mindex to maxdex-1 do
            let switch = sorterDef.switches.[i]
            let lv = slice.[switch.low]
            let hv = slice.[switch.hi]
            if(lv > hv) then
                slice.[switch.hi] <- lv
                slice.[switch.low] <- hv
                weights.[i] <- weights.[i] + 1


    let SortAllT (sorterDef:SorterDef) (testCases:Sortable[]) =
         let switchCount = (SwitchCount.value sorterDef.switchCount)
         let switchTracker = SwitchTracker.create sorterDef.switchCount
         let mutable i=0
         while (i < testCases.Length) do
                  SortT sorterDef 0 switchCount switchTracker testCases.[i]
                  i<-i+1
         switchTracker


    let SortTR (sorterDef:SorterDef) (mindex:int) (maxdex:int) 
               (switchTracker:SwitchTracker) (sortable:Sortable) =
        let span = new Span<int>(sortable.baseArray)
        let slice = span.Slice(sortable.offset, Degree.value(sortable.degree))
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


    let SortAllTR (sorterDef:SorterDef) (testCases:Sortable[]) =
         let switchCount = (SwitchCount.value sorterDef.switchCount)
         let switchTracker = SwitchTracker.create sorterDef.switchCount
         let mutable i=0
         let mutable success = true
         while (i < testCases.Length) do
                  success <- (SortTR sorterDef 0 switchCount switchTracker testCases.[i]) &&
                              success
                  i<-i+1
         switchTracker, success


    let SortTB (sorterDef:SorterDef) (mindex:int) (maxdex:int) 
               (switchUses:SwitchTracker) (lastSwitch:SwitchTracker) 
               (sortable:Sortable) =
        let span = new Span<int>(sortable.baseArray)
        let slice = span.Slice(sortable.offset, Degree.value(sortable.degree))
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


    let SortAllTB (sorterDef:SorterDef) (testCases:Sortable[]) =
         let switchCount = (SwitchCount.value sorterDef.switchCount)
         let switchUseTracker = SwitchTracker.create sorterDef.switchCount
         let lastSwitchTracker = SwitchTracker.create sorterDef.switchCount
         let mutable i=0
         while (i < testCases.Length) do
                  SortTB sorterDef 0 switchCount switchUseTracker lastSwitchTracker testCases.[i]
                  i<-i+1
         switchUseTracker, lastSwitchTracker
