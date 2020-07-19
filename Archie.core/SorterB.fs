namespace Archie.Base
open Microsoft.FSharp.Collections
open Sorting

module Sorter2 =
   
    // evaluates all sortables
    let UpdateSwitchTracker 
                (sorterDef:SorterDef)
                (switchTracker:SwitchTracker) 
                (startPos:int)
                (sortableSeq: seq<int[]>) =
        let rs (sortable:int[]) =
            SorterT.SwitchSeqOnSortableT
                sorterDef
                switchTracker
                {startPos .. (sorterDef.switches.Length - 1)}
                sortable
               
        sortableSeq |> Seq.map(rs) |> Seq.iter (fun i-> ())
        (SwitchUsage.CollectTheUsedSwitches sorterDef switchTracker)
   

    // returns early if a sort fails on any of the sortables
    let UpdateSwitchTrackerAndTest 
                (sorterDef:SorterDef)
                (switchTracker:SwitchTracker) 
                (startPos:int)
                (sortableSeq: seq<int[]>) =
   
        let rs (sortable:int[]) =
            SorterT.SwitchSeqOnSortableT
                sorterDef
                switchTracker
                {startPos .. (sorterDef.switches.Length - 1)}
                sortable

        let countFails b =
            if b then 0 else 1

        let failCount = sortableSeq 
                        |> Seq.map(rs)
                        |> Seq.fold(fun c seq -> c + ((Combinatorics.IsSorted seq) |> countFails)) 0
   
        (failCount, (Some (SwitchUsage.CollectTheUsedSwitches sorterDef switchTracker)))
   

    let TrackAndTest (sorterDef:SorterDef) =
        let startPos = 0
        let switchTracker = SwitchTracker.Make sorterDef.switchCount
        UpdateSwitchTrackerAndTest
                    sorterDef
                    switchTracker
                    startPos
                    (Sorting.SortableIntArray.SortableSeqAllBinary sorterDef.degree)
   
   
    let CondenseSortables 
        (sorterDef:SorterDef) 
        (sortableSeq:seq<int[]>) =
   
        let switchTracker = SwitchTracker.Make sorterDef.switchCount
        let (switchTracker, sortedItemsList) =  
            SorterT.CondenseSortableSeqWithSorter
                    sorterDef
                    switchTracker
                    sortableSeq
        (
            switchTracker,
            sortedItemsList // |> Seq.filter(fun stb -> not (Combinatorics.IsSorted stb))
        )
   
   
    let CondenseAllZeroOneSortables 
                (sorterDef:SorterDef) =
        CondenseSortables 
                    sorterDef 
                    (SortableIntArray.SortableSeqAllBinary sorterDef.degree)