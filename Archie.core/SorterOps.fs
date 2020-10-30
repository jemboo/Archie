namespace Archie.Base
open Microsoft.FSharp.Collections

type SorterTestResults = 
    {
        switchUses:SwitchUses;
        successfulSortCount:SortableCount;
        usedSwitchCount:SwitchCount;
        usedStageCount:StageCount
    }

module SorterTestResults = 
    let headers =
        [|"successfulSortCount"; "usedSwitchCount"; "usedStageCount"|]

    let report (sstr:SorterTestResults) =
        [|sprintf "%d" (SortableCount.value sstr.successfulSortCount);
          sprintf "%d" (SwitchCount.value sstr.usedSwitchCount);
          sprintf "%d" (StageCount.value sstr.usedStageCount);|]

    let reportOpt (sstr:SorterTestResults option) =
        match sstr with
        | Some r -> report r
        | None -> [|"";"";""|]


module SorterOps =

    let SwitchOnSortableIntArray (switch:Switch) 
                                 (sortableIntArray:SortableIntArray) =
        let intArray = SortableIntArray.value sortableIntArray
        let lv = intArray.[switch.low]
        let hv = intArray.[switch.hi]
        if(lv > hv) then
            let sCopy = sortableIntArray |> SortableIntArray.copy
            let copyInts = SortableIntArray.value sCopy
            copyInts.[switch.hi] <- lv
            copyInts.[switch.low] <- hv
            sCopy
        else sortableIntArray


    let rec SortableHistory (swtiches:Switch list)
                            (sortHistory:SortableIntArray list) =
        match swtiches with
        | [] -> sortHistory
        | swHead::swTail -> [(SwitchOnSortableIntArray swHead (sortHistory|> List.last))] |> List.append
                                (SortableHistory swTail sortHistory)

    let SwitchesOnSortableIntArray (swtiches:Switch list)
                                   (sortableIntArray:SortableIntArray) =
        let recList = (swtiches  |> List.rev)
        (SortableHistory swtiches  [sortableIntArray])


    let SortTHistSection2 (sorter:Sorter) (mindex:int) (maxdex:int)
                       (testCase:SortableIntArray) =
       let sws = sorter.switches |> Array.skip(mindex)
                                 |> Array.take(maxdex - mindex)
                                 |> Array.toList
       SwitchesOnSortableIntArray sws testCase

    let SortTHist2 (sorter:Sorter) (testCase:SortableIntArray) =
       let sl = SwitchCount.value sorter.switchCount
       SortTHistSection2 sorter 0 (sl - 1) testCase



    let SortTHistSwitches(switches:Switch list)
                      (testCase:SortableIntArray) =
        let mutable i = 0
        let mutable lstRet = [testCase]
        let mutable newCase = testCase

        while (i < switches.Length) do
            newCase <- newCase |> SortableIntArray.copy
            let intArray = SortableIntArray.value newCase
            let switch = switches.[i]
            let lv = intArray.[switch.low]
            let hv = intArray.[switch.hi]
            if(lv > hv) then
                intArray.[switch.hi] <- lv
                intArray.[switch.low] <- hv
            lstRet <- newCase::lstRet
            i <- i+1
        lstRet |> List.rev


    let SortTHistSwitchList (sorter:Sorter) (mindex:int) (maxdex:int) 
                         (testCase:SortableIntArray) =
       let sws = sorter.switches |> Array.skip(mindex)
                                 |> Array.take(maxdex - mindex)
                                 |> Array.toList
       SortTHistSwitches sws testCase


    let SortTHist (sorter:Sorter) (testCase:SortableIntArray) =
       let sl = SwitchCount.value sorter.switchCount
       SortTHistSwitchList sorter 0 (sl - 1) testCase


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
         let switchUses = SwitchUses.createEmpty sorter.switchCount
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
         let switchUses = SwitchUses.createEmpty sorter.switchCount
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
            SorterTestResults.usedStageCount = t;
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
         
