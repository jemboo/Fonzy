namespace global
open Microsoft.FSharp.Collections

type SortingResults = 
    {
        switchUses:SwitchUses;
        successfulSortCount:SortableCount;
        usedSwitchCount:SwitchCount;
        usedStageCount:StageCount
    }

module SortingResults = 
    let headers =
        [|"successfulSortCount"; "usedSwitchCount"; "usedStageCount"|]


    let makeStandard (s:Sorter) (su:SwitchUses) (sc:SortableCount) =
        let w, t = (SwitchUses.getSwitchAndStageUses s su)
        { 
            SortingResults.switchUses = su;
            SortingResults.successfulSortCount = sc;
            SortingResults.usedSwitchCount = w;
            SortingResults.usedStageCount = t;
        }

    let report (sstr:SortingResults) =
        [|sprintf "%d" (SortableCount.value sstr.successfulSortCount);
          sprintf "%d" (SwitchCount.value sstr.usedSwitchCount);
          sprintf "%d" (StageCount.value sstr.usedStageCount);|]

    let reportOpt (sstr:SortingResults option) =
        match sstr with
        | Some r -> report r
        | None -> [|"";"";""|]


module Eval =
    type SorterMethod =
         | AllSwitches
         | QuitWhenSorted

    module Sorter =
        let private fullSegment (sorter:Sorter) (mindex:int) (maxdex:int) 
                    (switchUses:SwitchUses) (ssRollup:SortableSetRollup) 
                    (index:int) =
            let useWeights = (SwitchUses.getWeights switchUses)
            let mutable i = mindex
            while (i < maxdex) do
                let switch = sorter.switches.[i]
                let lv = ssRollup.baseArray.[switch.low + index]
                let hv = ssRollup.baseArray.[switch.hi + index]
                if(lv > hv) then
                    ssRollup.baseArray.[switch.hi + index] <- lv
                    ssRollup.baseArray.[switch.low + index] <- hv
                    useWeights.[i] <- useWeights.[i] + 1
                i <- i+1
            Combinatorics.isSortedOffset ssRollup.baseArray index (Degree.value(ssRollup.degree))


        let fullRollup (sorter:Sorter) (ssRollup:SortableSetRollup) =
             let switchCount = (SwitchCount.value sorter.switchCount)
             let switchUses = SwitchUses.createEmpty sorter.switchCount
             let tcCopy = (SortableSetRollup.copy ssRollup) |> Result.ExtractOrThrow
             let mutable i=0
             let mutable successCount = 0
             while (i < ssRollup.baseArray.Length) do
                      successCount  <- (if (fullSegment sorter 0 switchCount switchUses tcCopy i) then 1 else 0) +
                                      successCount
                      i <- i + (Degree.value sorter.degree)
             switchUses, SortableCount.fromInt successCount


        // returns early when the sortable is sorted
        let private checkSegment (sorter:Sorter) (mindex:int) (maxdex:int) 
                   (switchUses:SwitchUses) (testCases:SortableSetRollup) (index:int) =
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


        let checkRollup (sorter:Sorter) (testCases:SortableSetRollup) =
             let switchCount = (SwitchCount.value sorter.switchCount)
             let switchUses = SwitchUses.createEmpty sorter.switchCount
             let tcCopy = (SortableSetRollup.copy testCases) |> Result.ExtractOrThrow
             let mutable i=0
             let mutable successCount = 0
             while (i < testCases.baseArray.Length) do
                      successCount  <- (if (checkSegment sorter 0 switchCount switchUses tcCopy i) then 1 else 0) +
                                          successCount
                      i <- i + (Degree.value sorter.degree)
             switchUses, SortableCount.fromInt successCount
   
        let private makeResults (sortableSet:SortableSetRollup) (sorter:Sorter) 
                                sortingProc =
            let su, sc = sortingProc sorter sortableSet
            SortingResults.makeStandard sorter su sc

        let GetResults (sortableSet:SortableSetRollup)
                       (sorter:Sorter) (sorterMethod:SorterMethod) =
            match sorterMethod with
            | AllSwitches -> fullRollup |> makeResults sortableSet sorter
            | QuitWhenSorted -> checkRollup |> makeResults sortableSet sorter


    module SorterSet =

        let private makeResults (sortableSet:SortableSetRollup) (sorters:Sorter[]) 
                                (_parallel:UseParallel) sortingProc =
            let rewrap s = 
                let su, sc = sortingProc s sortableSet
                s, (SortingResults.makeStandard s su sc)

            match UseParallel.value(_parallel) with
            | true -> sorters |> Array.Parallel.map(fun s-> rewrap s)
            | false -> sorters |> Array.map(fun s-> rewrap s)

        let GetResults (sortableSet:SortableSetRollup) (_parallel:UseParallel)
                       (sorters:Sorter[]) (sorterMethod:SorterMethod) =
            match sorterMethod with
            | AllSwitches -> Sorter.fullRollup |> makeResults sortableSet sorters _parallel
            | QuitWhenSorted -> Sorter.checkRollup |> makeResults sortableSet sorters _parallel
         

        module History =

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

