namespace Archie.Base
open Microsoft.FSharp.Collections

type SorterTestResults = 
    {
        switchUses:SwitchUses;
        successfulSortCount:SortableCount;
        usedSwitchCount:SwitchCount;
        stageUseCount:StageCount
    }

module SorterTestResults = 
    let headers =
        [|"successfulSortCount"; "usedSwitchCount"; "stageUseCount"|]

    let report (sstr:SorterTestResults) =
        [|sprintf "%d" (SortableCount.value sstr.successfulSortCount);
          sprintf "%d" (SwitchCount.value sstr.usedSwitchCount);
          sprintf "%d" (StageCount.value sstr.stageUseCount);|]

    let reportOpt (sstr:SorterTestResults option) =
        match sstr with
        | Some r -> report r
        | None -> [|"";"";""|]


module SorterOps =

    let SortTR (sorter:Sorter) (mindex:int) (maxdex:int) 
               (switchUses:SwitchUses) (testCases:SortableSet) 
               (index:int) =
        let useWeights = (SwitchUses.getWeights switchUses)
        let mutable i = mindex
        while (i < maxdex) do
            let switch = sorter.switches.[i]
            let lv = testCases.baseArray.[switch.low + index]
            let hv = testCases.baseArray.[switch.hi + index]
            if(lv > hv) then
                testCases.baseArray.[switch.hi + index] <- lv
                testCases.baseArray.[switch.low + index] <- hv
                useWeights.[i] <- useWeights.[i] + 1
            i <- i+1
        Combinatorics.isSortedOffset testCases.baseArray index (Degree.value(testCases.degree))


    let SortAllComplete (sorter:Sorter) (testCases:SortableSet) =
         let switchCount = (SwitchCount.value sorter.switchCount)
         let switchUses = SwitchUses.create sorter.switchCount
         let tcCopy = (SortableSet.copy testCases) |> Result.ExtractOrThrow
         let mutable i=0
         let mutable successCount = 0
         while (i < testCases.baseArray.Length) do
                  successCount  <- (if (SortTR sorter 0 switchCount switchUses tcCopy i) then 1 else 0) +
                                  successCount
                  i <- i + (Degree.value sorter.degree)
         switchUses, SortableCount.fromInt successCount


    // returns early when the sortable is sorted
    let SortTB (sorter:Sorter) (mindex:int) (maxdex:int) 
               (switchUses:SwitchUses) (testCases:SortableSet) (index:int) =
        let useWeights = (SwitchUses.getWeights switchUses)
        let mutable looP = true
        let mutable i = mindex
        while ((i < maxdex) && looP) do
            let switch = sorter.switches.[i]
            let lv = testCases.baseArray.[switch.low + index]
            let hv = testCases.baseArray.[switch.hi + index]
            if(lv > hv) then
                testCases.baseArray.[switch.hi + index] <- lv
                testCases.baseArray.[switch.low + index] <- hv
                useWeights.[i] <- useWeights.[i] + 1
                looP <- not (Combinatorics.isSortedOffset testCases.baseArray 
                               index (Degree.value(testCases.degree))) 
            i <- i+1
        Combinatorics.isSortedOffset testCases.baseArray index (Degree.value(testCases.degree))


    let SortAllEager (sorter:Sorter) (testCases:SortableSet) =
         let switchCount = (SwitchCount.value sorter.switchCount)
         let switchUses = SwitchUses.create sorter.switchCount
         let tcCopy = (SortableSet.copy testCases) |> Result.ExtractOrThrow
         let mutable i=0
         let mutable successCount = 0
         while (i < testCases.baseArray.Length) do
                  successCount  <- (if (SortTB sorter 0 switchCount switchUses tcCopy i) then 1 else 0) +
                                      successCount
                  i <- i + (Degree.value sorter.degree)
         switchUses, SortableCount.fromInt successCount
   

    let makeStandardSorterTestResults (s:Sorter) (su:SwitchUses) (sc:SortableCount) =
        let w, t = (SwitchUses.getSwitchAndStageUses s su)
        { 
            SorterTestResults.switchUses = su;
            SorterTestResults.successfulSortCount = sc;
            SorterTestResults.usedSwitchCount = w;
            SorterTestResults.stageUseCount = t;
        }

    let private GetTheStandardSortingResults (sortableSet:SortableSet) (sorter:Sorter) 
                                             sorterOnSortableSet =
        let su, sc = sorterOnSortableSet sorter sortableSet
        makeStandardSorterTestResults sorter su sc

    let GetTheStandardSortingResultsComplete (sortableSet:SortableSet)
                                             (sorter:Sorter) =
        SortAllComplete |> GetTheStandardSortingResults sortableSet sorter

    let GetTheStandardSortingResultsEager (sortableSet:SortableSet)
                                          (sorter:Sorter) =
        SortAllEager |> GetTheStandardSortingResults sortableSet sorter


    let private GetStandardSortingResults (sortableSet:SortableSet) (sorters:Sorter[]) 
                                          (_parallel:UseParallel) sorterOnSortableSet =
        let rewrap s = 
            let su, sc = sorterOnSortableSet s sortableSet
            s, (makeStandardSorterTestResults s su sc)

        match UseParallel.value(_parallel) with
        | true -> sorters |> Array.Parallel.map(fun s-> rewrap s)
        | false -> sorters |> Array.map(fun s-> rewrap s)


    let GetStandardSortingResultsComplete (sortableSet:SortableSet) (_parallel:UseParallel) 
                                          (sorters:Sorter[]) =
        SortAllComplete |> GetStandardSortingResults sortableSet sorters _parallel


    let GetStandardSortingResultsEager (sortableSet:SortableSet) (_parallel:UseParallel)
                                       (sorters:Sorter[]) =
        SortAllEager |> GetStandardSortingResults sortableSet sorters _parallel
         
