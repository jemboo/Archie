namespace Archie.Base
open System
open Microsoft.FSharp.Collections
open Sorting

//These routines mutate the sortable inputs
module SorterX =

   let Sort (sorterDef:SorterDef) (mindex:int) (maxdex:int) (sortable:int[]) =
       for i=mindex to maxdex-1 do
           let switch = sorterDef.switches.[i]
           let lv = sortable.[switch.low]
           let hv = sortable.[switch.hi]
           if(lv > hv) then
               sortable.[switch.hi] <- lv
               sortable.[switch.low] <- hv

   let SortAndReturn (sorterDef:SorterDef) (mindex:int) (maxdex:int) (sortable:int[]) =
    for i=mindex to maxdex-1 do
        let switch = sorterDef.switches.[i]
        let lv = sortable.[switch.low]
        let hv = sortable.[switch.hi]
        if(lv > hv) then
            sortable.[switch.hi] <- lv
            sortable.[switch.low] <- hv
    sortable

   // returns early if a sort fails on any of the sortables
   let private SortAndQuitOnFail (sorterDef:SorterDef) (testCases:Sortable[]) =
         let switchCount = (SwitchCount.value sorterDef.switchCount)
         let mutable i=0
         let mutable looP = true
         while ((i < testCases.Length) && looP) do
              let intsOut = Sortable.value testCases.[i]
              let res = SortAndReturn sorterDef 0 switchCount intsOut
              looP <- (Combinatorics.IsSorted res)
              i<-i+1
         Some looP

   let private SortAll (sorterDef:SorterDef) (testCases:Sortable[]) =
          let switchCount = (SwitchCount.value sorterDef.switchCount)
          let mutable i=0
          while (i < testCases.Length) do
               let intsOut = Sortable.value testCases.[i]
               Sort sorterDef 0 switchCount intsOut
               i<-i+1
          None

   // returns early if a sort fails on any of the sortables
   let SortTheCases (sorterDef:SorterDef) (testCases:Sortable[]) (quitOnFail:bool) =
        match quitOnFail with
        | true -> SortAndQuitOnFail sorterDef testCases
        | false -> SortAll sorterDef testCases

    // returns early if a sort fails on any of the sortables
   let SortAllBinaries (sorterDef:SorterDef) =
          SortTheCases sorterDef (Sortable.AllBinary sorterDef.degree) true


//These routines work on and return a copy the sortable inputs
module SorterC =

   let SortTheCases (sorterDef:SorterDef) (testCases:Sortable[]) =
        let switchCount = (SwitchCount.value sorterDef.switchCount)
        let mutable i=0
        seq { while (i < testCases.Length) do
                 let sir = Sortable.copy testCases.[i]
                 let intsOut = Sortable.value sir
                 yield (SorterX.Sort sorterDef 0 switchCount intsOut)
                 i<-i+1
            }

   let SortAllBinaries (sorterDef:SorterDef) =
           SortTheCases sorterDef (Sortable.AllBinary sorterDef.degree)


module SorterR =
    let Sort (sorterDef:SorterDef) (mindex:int) (maxdex:int) 
             (sortReaction:int->unit) (sortable:int[]) =
        for i=mindex to maxdex-1 do
            let switch = sorterDef.switches.[i]
            let lv = sortable.[switch.low]
            let hv = sortable.[switch.hi]
            if(lv > hv) then
                sortable.[switch.hi] <- lv
                sortable.[switch.low] <- hv
                sortReaction i

    let SortTheCasesAndTrack (sorterDef:SorterDef) (testCases:Sortable[]) =
         let switchCount = (SwitchCount.value sorterDef.switchCount)
         let switchTracker = SwitchTracker.create sorterDef.switchCount
         let weights = (SwitchTracker.weights switchTracker)
         let sortReaction = fun i -> weights.[i] <- weights.[i] + 1
         let mutable i=0
         while (i < testCases.Length) do
                  let intsOut = Sortable.value (testCases.[i])
                  Sort sorterDef 0 switchCount sortReaction intsOut
                  i<-i+1
         switchTracker

    let SortAllBinariesAndTrack (sorterDef:SorterDef) =
            SortTheCasesAndTrack sorterDef (Sortable.AllBinary sorterDef.degree)



module SorterT2 =
    let SwitchSeqOnSortable (sorterDef:SorterDef) (switchIndexes:seq<int>)
                            (sortReaction:int->unit) (sortable:Sortable) =
        let sir = Sortable.copy sortable
        let intsOut = Sortable.value sortable
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
                               (sortables:Sortable[] option) =
           let indexes = match sorterIndexes with
                            | Some indexes -> indexes
                            | None ->  Array.init(sorterDef.switches.Length) (fun i -> i)

           let sortables = match sortables with
                              | Some sortables -> sortables
                              | None ->  (Sortable.AllBinary sorterDef.degree)

           let switchTracker = SwitchTracker.create sorterDef.switchCount
           let weights = (SwitchTracker.weights switchTracker)
           let sortReaction = fun i -> weights.[i] <- weights.[i] + 1
           let sortFailsPairs  = sortables |> Seq.map(SwitchSeqOnSortable sorterDef indexes sortReaction)
                                           |> Seq.filter(fun s -> not (Combinatorics.IsSorted (Sortable.value (snd s))))
                                           |> Seq.toArray
           sortFailsPairs, switchTracker