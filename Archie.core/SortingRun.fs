namespace Archie.Base
open System
open SorterParts

module SortingRun =

    let private SorterSetOnSortableSet (sortableSet:SortableSet) (sorterSet:SorterSet) 
                                       (_parallel:bool) sorterOnSortableSet =
        match _parallel with
        | true -> sorterSet.sorters |> Array.Parallel.map(
                        fun s-> sorterOnSortableSet s sortableSet)
        | false -> sorterSet.sorters |> Array.map(
                        fun s-> sorterOnSortableSet s sortableSet)


    let CompleteSort (sortableSet:SortableSet) (sorterSet:SorterSet) 
                     (_parallel:bool) =
        SorterOps.SortAllTR |> SorterSetOnSortableSet sortableSet sorterSet _parallel


    let StopIfSorted (sortableSet:SortableSet) (sorterSet:SorterSet) 
                     (_parallel:bool) =
        SorterOps.SortAllTB |> SorterSetOnSortableSet sortableSet sorterSet _parallel
