namespace Archie.Base
open Microsoft.FSharp.Collections
open Sorting
open Combinatorics_Types

 module SorterX =
    let SwitchSeqOnSortable (sorterDef:SorterDef) (switchIndexes:seq<int>)
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
  
    let SorterOnSortable (sorterDef:SorterDef) (sortable:int[]) =
             SwitchSeqOnSortable sorterDef { 0 .. (sorterDef.switches.Length - 1) }             
                                 sortable

    // returns early if a sort fails on any of the sortables
    let SorterFullTestPassFail (sorterDef:SorterDef) =
          (IntBits.AllBinaryTestCases (Degree.value(sorterDef.degree)))
               |> Seq.map(fun s-> SorterOnSortable sorterDef s)
               |> Seq.forall(fun s -> Combinatorics.IsSorted s)


 module SorterC =
    let SwitchSeqOnSortable (sorterDef:SorterDef) (switchIndexes:seq<int>) (sortable:SortableIntArray) =
        let sir = SortableIntArray.copy sortable
        let intsOut = SortableIntArray.value sir
        switchIndexes
            |> Seq.iter(fun i ->
                let switch = sorterDef.switches.[i]
                let lv = intsOut.[switch.low]
                let hv = intsOut.[switch.hi]
                if(lv > hv) then
                    intsOut.[switch.hi] <- lv
                    intsOut.[switch.low] <- hv)
        sortable, sir

    let CollectFails (sorterDef:SorterDef) (sorterIndexes:int[] option) (sortableSeq: seq<SortableIntArray> option) =
           let indexes = match sorterIndexes with
                            | Some indexes -> indexes
                            | None ->  Array.init(sorterDef.switches.Length) (fun i -> i)

           let sortables = match sortableSeq with
                             | Some sortables -> sortables
                             | None ->  (Sorting.SortableIntArray.AllBinary sorterDef.degree)

           sortables |> Seq.map(SwitchSeqOnSortable sorterDef indexes)
                       |> Seq.filter(fun s -> not (Combinatorics.IsSorted (SortableIntArray.value (snd s))))

module SorterT =
    let SwitchSeqOnSortable (sorterDef:SorterDef) (switchIndexes:seq<int>)
                            (sortReaction:int->unit) (sortable:SortableIntArray) =
        let sir = SortableIntArray.copy sortable
        let intsOut = SortableIntArray.value sortable
        switchIndexes
            |> Seq.iter(fun i ->
                let switch = sorterDef.switches.[i]
                let lv = intsOut.[switch.low]
                let hv = intsOut.[switch.hi]
                if(lv > hv) then
                    intsOut.[switch.hi] <- lv
                    intsOut.[switch.low] <- hv
                    sortReaction i)
        sortable, sir

    let CollectFailsAndTracker (sorterDef:SorterDef) (sorterIndexes:int[] option) 
                               (sortableSeq:seq<SortableIntArray> option) =
           let indexes = match sorterIndexes with
                            | Some indexes -> indexes
                            | None ->  Array.init(sorterDef.switches.Length) (fun i -> i)

           let sortables = match sortableSeq with
                              | Some sortables -> sortables
                              | None ->  (Sorting.SortableIntArray.AllBinary sorterDef.degree)

           let switchTracker = SwitchTracker.create sorterDef.switchCount
           let weights = (SwitchTracker.weights switchTracker)
           let sortReaction = fun i -> weights.[i] <- weights.[i] + 1
           let sortFailsPairs  = sortables |> Seq.map(SwitchSeqOnSortable sorterDef indexes sortReaction)
                                           |> Seq.filter(fun s -> not (Combinatorics.IsSorted (SortableIntArray.value (snd s))))
                                           |> Seq.toArray
           sortFailsPairs, switchTracker