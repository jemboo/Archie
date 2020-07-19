namespace Archie.Base
open Microsoft.FSharp.Collections
open Sorting

 module Sorter =

    let SwitchSeqOnSortable
                    (sorterDef:SorterDef)
                    (switchIndexes:seq<int>)
                    (sortable:int[]) =
        switchIndexes
            |> Seq.iter(fun i ->
                let switch = sorterDef.switches.[i]
                let lv = sortable.[switch.low]
                let hv = sortable.[switch.hi]
                if(lv > hv) then
                    sortable.[switch.hi] <- lv
                    sortable.[switch.low] <- hv)
        sortable
   

    let SorterOnSortable
                (sorterDef:SorterDef)
                (sortable:int[]) =
            SwitchSeqOnSortable
                sorterDef
                { 0 .. (sorterDef.switches.Length - 1) }             
                sortable

   
    // returns early if a sort fails on any of the sortables
    let EvalSortableSeq
                   (sorterDef:SorterDef)
                   (sortableSeq: seq<int[]>) =
   
           sortableSeq 
                |> Seq.map(SorterOnSortable sorterDef)
                |> Seq.forall(Combinatorics.IsSorted)

    let EvalSorter (sorterDef:SorterDef) =
        EvalSortableSeq
            sorterDef
            (Sorting.SortableIntArray.SortableSeqAllBinary sorterDef.degree)



module SorterC =

   let SwitchSeqOnSortable
                   (sorterDef:SorterDef)
                   (switchIndexes:seq<int>)
                   (sortable:int[]) =

       let sc = sortable |> Array.copy
       switchIndexes
           |> Seq.iter(fun i ->
               let switch = sorterDef.switches.[i]
               let lv = sc.[switch.low]
               let hv = sc.[switch.hi]
               if(lv > hv) then
                   sc.[switch.hi] <- lv
                   sc.[switch.low] <- hv)
       sc
  

   let SorterOnSortable
               (sorterDef:SorterDef)
               (sortable:int[]) =
           SwitchSeqOnSortable
               sorterDef
               { 0 .. (sorterDef.switches.Length - 1) }             
               sortable

  
   // returns early if a sort fails on any of the sortables
   let EvalSortableSeq
                  (sorterDef:SorterDef)
                  (sortableSeq: seq<int[]>) =
  
          sortableSeq 
               |> Seq.map(SorterOnSortable sorterDef)
               |> Seq.forall(Combinatorics.IsSorted)

   let EvalSorter (sorterDef:SorterDef) =
       EvalSortableSeq
           sorterDef
           (Sorting.SortableIntArray.SortableSeqAllBinary sorterDef.degree)





module SorterT =
    let SwitchSeqOnSortableT
                    (sorterDef:SorterDef) 
                    (switchTracker:SwitchTracker)
                    (switchIndexes:seq<int>)
                    (sortable:int[]) =
        let sc = sortable |> Array.copy
        switchIndexes
            |> Seq.iter(fun i ->
                let switch = sorterDef.switches.[i]
                let lv = sc.[switch.low]
                let hv = sc.[switch.hi]
                if(lv > hv) then
                    sc.[switch.hi] <- lv
                    sc.[switch.low] <- hv
                    switchTracker.weights.[i] <-
                        switchTracker.weights.[i] + 1)
        sc


    let CondenseSortableSeqWithSwitchSeq
                 (sorterDef:SorterDef) 
                 (switchTracker:SwitchTracker) 
                 (switchIndexes:seq<int>)
                 (sortableSeq: seq<int[]>) = 

         let arrayOfCondensedSortables 
                 = sortableSeq   |> Seq.map(SwitchSeqOnSortableT sorterDef 
                                             switchTracker switchIndexes)
                                 |> Seq.countBy id
                                 |> Seq.map fst
                                 |> Seq.toArray

         (switchTracker, arrayOfCondensedSortables)


    let CondenseSortableSeqWithSorter 
                   (sorterDef:SorterDef) 
                   (switchTracker:SwitchTracker) 
                   (sortableSeq: seq<int[]>) = 
  
           CondenseSortableSeqWithSwitchSeq
                       sorterDef
                       switchTracker
                       {0 .. (sorterDef.switches.Length - 1)}
                       sortableSeq


    let CondenseSortables 
        (sorterDef:SorterDef) 
        (sortableSeq:seq<int[]>) =
   
        let switchTracker = SwitchTracker.Make sorterDef.switchCount
        let (switchTracker, sortedItemsList) =  
            CondenseSortableSeqWithSorter
                    sorterDef
                    switchTracker
                    sortableSeq
        (
            switchTracker,
            sortedItemsList // |> Seq.filter(fun stb -> not (Combinatorics.IsSorted stb))
        )
   

    let CondenseAllZeroOneSortables (sorterDef:SorterDef) =
        CondenseSortables 
                    sorterDef 
                    (SortableIntArray.SortableSeqAllBinary sorterDef.degree)



    let SorterOnSortableT
                (sorterDef:SorterDef) 
                (switchTracker:SwitchTracker) 
                (sortable:int[]) =
            SwitchSeqOnSortableT
                sorterDef 
                switchTracker
                { 0 .. (sorterDef.switches.Length - 1) }             
                sortable
   
   
   
    // returns early if a sort fails on any of the sortables
    let EvalSortableSeqT 
                (sorterDef:SorterDef)
                (switchTracker:SwitchTracker)
                (sortableSeq: seq<int[]>) =
   
        let allGood = sortableSeq 
                        |> Seq.map(SorterOnSortableT sorterDef switchTracker)
                        |> Seq.forall(Combinatorics.IsSorted)
   
        if allGood then
            (allGood, Some (SwitchUsage.CollectTheUsedSwitches sorterDef switchTracker))
        else (allGood, None)
   
   
    let EvalSorterT (sorterDef:SorterDef) =
       let switchTracker = SwitchTracker.Make sorterDef.switchCount
       EvalSortableSeqT
           sorterDef
           switchTracker
           (Sorting.SortableIntArray.SortableSeqAllBinary sorterDef.degree)

