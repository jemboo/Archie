namespace Archie.Base
open System
open SorterParts


module SortingRun2 =

    let RunSorterSetOnSortableSetTR (sortableSet:SortableSet3) (sorterSet:SorterSet) =
        let RunAndReset sorter sortableSet =
            let res = SorterOps2.SortAllTR sorter sortableSet
            res
        sorterSet.sorters |> Array.map(fun s-> RunAndReset s sortableSet)
        

    let RunSorterSetOnSortableSetTB (sortableSet:SortableSet) (sorterSet:SorterSet) =
        let RunAndReset sorter sortableSet =
            let res = SorterOps2.SortAllTB sorter sortableSet
            res
        sorterSet.sorters |> Array.map(fun s-> RunAndReset s sortableSet)

    let RunSorterSetOnSortableSetTBp (sortableSet:SortableSet) (sorterSet:SorterSet) =
        let RunAndReset sorter sortableSet =
            let res = SorterOps2.SortAllTB sorter sortableSet
            res
        sorterSet.sorters |> Array.Parallel.map(fun s-> RunAndReset s sortableSet)


    let RunSorterSetOnSortableSetTB2 (sortableSet:SortableSet) (sorterSet:SorterSetE) =
        let RunAndReset sorter sortableSet =
            let res = SorterOps2.SortAllTB (Entity.character sorter) sortableSet
            Dependent.create sorter res
        sorterSet.sorters |> Array.map(fun s-> RunAndReset s sortableSet)


    let RunSorterSetOnSortableSetTB2p (sortableSet:SortableSet) (sorterSet:SorterSetE) =
        let RunAndReset sorter sortableSet =
            let res = SorterOps2.SortAllTB (Entity.character sorter) sortableSet
            Dependent.create sorter res
        sorterSet.sorters |> Array.Parallel.map(fun s-> RunAndReset s sortableSet)


module SortingRun =

    let RunSorterSetOnSortableSetTR (sortableSet:SortableSet3) (sorterSet:SorterSet) =
        let RunAndReset sorter sortableSet =
            let res = SorterOps3.SortAllTR sorter sortableSet
            res
        sorterSet.sorters |> Array.map(fun s-> RunAndReset s sortableSet)
        

    let RunSorterSetOnSortableSetTB (sortableSet:SortableSet3) (sorterSet:SorterSet) =
        let RunAndReset sorter sortableSet =
            let res = SorterOps3.SortAllTB sorter sortableSet
            res
        sorterSet.sorters |> Array.map(fun s-> RunAndReset s sortableSet)


    let RunSorterSetOnSortableSetTBp (sortableSet:SortableSet3) (sorterSet:SorterSet) =
        let RunAndReset sorter sortableSet =
            let res = SorterOps3.SortAllTB sorter sortableSet
            res
        sorterSet.sorters |> Array.Parallel.map(fun s-> RunAndReset s sortableSet)


    let RunSorterSetOnSortableSetTB2 (sortableSet:SortableSet3) (sorterSet:SorterSetE) =
        let RunAndReset sorter sortableSet =
            let res = SorterOps3.SortAllTB (Entity.character sorter) sortableSet
            Dependent.create sorter res
        sorterSet.sorters |> Array.map(fun s-> RunAndReset s sortableSet)


    let RunSorterSetOnSortableSetTB2p (sortableSet:SortableSet3) (sorterSet:SorterSetE) =
        let RunAndReset sorter sortableSet =
            let res = SorterOps3.SortAllTB (Entity.character sorter) sortableSet
            Dependent.create sorter res
        sorterSet.sorters |> Array.Parallel.map(fun s-> RunAndReset s sortableSet)
