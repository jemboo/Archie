namespace Archie.Base
open System
open SorterParts

module SortingRun =

    let RunSorterSetOnSortableSetTR (sortableSet:SortableSet) (sorterSet:SorterSet) =
        let RunAndReset sorter sortableSet =
            let res = SorterOps.SortAllTR sorter sortableSet.sortables
            SortableSet.Reset sortableSet
            res
        sorterSet.sorters |> Array.map(fun s-> RunAndReset s sortableSet)
        

    let RunSorterSetOnSortableSetTB (sortableSet:SortableSet) (sorterSet:SorterSet) =
        let RunAndReset sorter sortableSet =
            let res = SorterOps.SortAllTB sorter sortableSet.sortables
            SortableSet.Reset sortableSet
            res
        sorterSet.sorters |> Array.map(fun s-> RunAndReset s sortableSet)


    let RunSorterSetOnSortableSetTBp (sortableSet:SortableSet) (sorterSet:SorterSet) =
        let RunAndReset sorter sortableSet =
            let res = SorterOps.SortAllTB sorter sortableSet.sortables
            SortableSet.Reset sortableSet
            res
        sorterSet.sorters |> Array.Parallel.map(fun s-> RunAndReset s sortableSet)


    let RunSorterSetOnSortableSetTB2 (sortableSet:SortableSet) (sorterSet:SorterSet2) =
        let RunAndReset sorter sortableSet =
            let res = SorterOps.SortAllTB (Entity.character sorter) sortableSet.sortables
            SortableSet.Reset sortableSet
            Dependent.create sorter res
        sorterSet.sorters |> Array.map(fun s-> RunAndReset s sortableSet)


    let RunSorterSetOnSortableSetTB2p (sortableSet:SortableSet) (sorterSet:SorterSet2) =
        let RunAndReset sorter sortableSet =
            let res = SorterOps.SortAllTB (Entity.character sorter) sortableSet.sortables
            SortableSet.Reset sortableSet
            Dependent.create sorter res
        sorterSet.sorters |> Array.Parallel.map(fun s-> RunAndReset s sortableSet)


//module SortingRun2 =

//    let RunSorterSetOnSortableSetTR (sortableSet:SortableSet2) (sorterSet:SorterSet) =
//        let RunAndReset sorter sortableSet =
//            let res = SorterOps.SortAllTR sorter sortableSet.sortables
//            SortableSet.Reset sortableSet
//            res
//        sorterSet.sorters |> Array.map(fun s-> RunAndReset s sortableSet)
        

//    let RunSorterSetOnSortableSetTB (sortableSet:SortableSet2) (sorterSet:SorterSet) =
//        let RunAndReset sorter sortableSet =
//            let res = SorterOps.SortAllTB sorter sortableSet.sortables
//            SortableSet.Reset sortableSet
//            res
//        sorterSet.sorters |> Array.map(fun s-> RunAndReset s sortableSet)


//    let RunSorterSetOnSortableSetTBp (sortableSet:SortableSet2) (sorterSet:SorterSet) =
//        let RunAndReset sorter sortableSet =
//            let res = SorterOps.SortAllTB sorter sortableSet.sortables
//            SortableSet.Reset sortableSet
//            res
//        sorterSet.sorters |> Array.Parallel.map(fun s-> RunAndReset s sortableSet)


//    let RunSorterSetOnSortableSetTB2 (sortableSet:SortableSet2) (sorterSet:SorterSet2) =
//        let RunAndReset sorter sortableSet =
//            let res = SorterOps.SortAllTB (Entity.character sorter) sortableSet.sortables
//            SortableSet.Reset sortableSet
//            Dependent.create sorter res
//        sorterSet.sorters |> Array.map(fun s-> RunAndReset s sortableSet)


//    let RunSorterSetOnSortableSetTB2p (sortableSet:SortableSet2) (sorterSet:SorterSet2) =
//        let RunAndReset sorter sortableSet =
//            let res = SorterOps.SortAllTB (Entity.character sorter) sortableSet.sortables
//            SortableSet.Reset sortableSet
//            Dependent.create sorter res
//        sorterSet.sorters |> Array.Parallel.map(fun s-> RunAndReset s sortableSet)
