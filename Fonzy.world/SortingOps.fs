namespace global
open System


module SortingOps =
    // applies every switch between mindex and maxdex to the sortable
    let private makeSwitchUsesRolloutSegment (sorter:Sorter) (mindex:int) (maxdex:int) 
                (ssRollout:SortableSetRollout) (useTrack:int[]) (utIndex:int)
                (index:int) =
        let mutable i = mindex
        while (i < maxdex) do
            let switch = sorter.switches.[i]
            let lv = ssRollout.baseArray.[switch.low + index]
            let hv = ssRollout.baseArray.[switch.hi + index]
            if(lv > hv) then
                ssRollout.baseArray.[switch.hi + index] <- lv
                ssRollout.baseArray.[switch.low + index] <- hv
                useTrack.[i + utIndex] <- 1
            i <- i + 1

    let makeSwitchUsesRollout (sorter:Sorter) (ssRollout:SortableSetRollout) 
                              (switchusePlan:SwitchusePlan) =
        let switchCount = (SwitchCount.value sorter.switchCount)
        let firstSwitchDex, lastSwitchDex = 
            match switchusePlan with
            | All -> (0, switchCount)
            | Range (min, max) -> (min, max)
        let tcCopy = (SortableSetRollout.copy ssRollout) |> Result.ExtractOrThrow
        let swUseR =SwitchUseRollout.create sorter.switchCount ssRollout.sortableCount
        let mutable i=0
        let mutable ut=0
        while (i < ssRollout.baseArray.Length) do
                makeSwitchUsesRolloutSegment sorter firstSwitchDex 
                        lastSwitchDex tcCopy swUseR.useRoll ut i
                i <- i + (Degree.value sorter.degree)
                ut <- ut + (SwitchCount.value sorter.switchCount)
        swUseR, tcCopy

    // applies every switch between mindex and maxdex to the sortable
    let private makeSwitchUsesSegment (sorter:Sorter) (mindex:int) (maxdex:int) 
                (switchUses:SwitchUses) (ssRollout:SortableSetRollout) 
                (index:int) =
        let useWeights = (SwitchUses.getWeights switchUses)
        let mutable i = mindex
        while (i < maxdex) do
            let switch = sorter.switches.[i]
            let lv = ssRollout.baseArray.[switch.low + index]
            let hv = ssRollout.baseArray.[switch.hi + index]
            if(lv > hv) then
                ssRollout.baseArray.[switch.hi + index] <- lv
                ssRollout.baseArray.[switch.low + index] <- hv
                useWeights.[i] <- useWeights.[i] + 1
            i <- i+1

    let makeSwitchUses (sorter:Sorter) (ssRollout:SortableSetRollout)
                       (switchusePlan:SwitchusePlan) =
        let switchCount = (SwitchCount.value sorter.switchCount)
        let firstSwitchDex, lastSwitchDex = 
            match switchusePlan with
            | All -> (0, switchCount)
            | Range (min, max) -> (min, max)
        let switchUses = SwitchUses.createEmpty sorter.switchCount
        let tcCopy = (SortableSetRollout.copy ssRollout) |> Result.ExtractOrThrow
        let mutable i=0
        while (i < ssRollout.baseArray.Length) do
                makeSwitchUsesSegment 
                    sorter firstSwitchDex lastSwitchDex switchUses tcCopy i
                i <- i + (Degree.value sorter.degree)
        switchUses, tcCopy

    // returns early when the sortable is sorted
    let private returnWhenSortedSegment (sorter:Sorter) (mindex:int) (maxdex:int) 
                (switchUses:SwitchUses) (testCases:SortableSetRollout) (index:int) =
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

    let returnWhenSortedRollout (sorter:Sorter) (ssRollout:SortableSetRollout) =
        let switchCount = (SwitchCount.value sorter.switchCount)
        let switchUses = SwitchUses.createEmpty sorter.switchCount
        let tcCopy = (SortableSetRollout.copy ssRollout) |> Result.ExtractOrThrow
        let mutable i=0
        while (i < ssRollout.baseArray.Length) do
                returnWhenSortedSegment sorter 0 switchCount switchUses tcCopy i |> ignore
                i <- i + (Degree.value sorter.degree)
        switchUses, tcCopy

    let evalSorter (sorter:Sorter) (sortingStrategy:SortingStrategy)
                   (switchusePlan:SwitchusePlan) (sorterEvalReporting:SorterEvalReporting) =
        None
    //let GetResults (sortableSet:SortableSetRollout) (sorter:Sorter) (sorterId:Guid) 
    //                (sortableSetId:Guid) (sorterEval:SortingStrategy) =
    //    match sorterEval with
    //    | RunAllSwitches -> fullRollout |> makeResults sortableSet sorter sorterId sortableSetId
    //    | QuitWhenSorted -> checkRollout |> makeResults sortableSet sorter sorterId sortableSetId


    module SorterSet =

        let private makeResults (sortableSet:SortableSetRollout) (sorterSet:SorterSet) 
                                (sorterId:Guid) (sortableSetId:Guid) (_parallel:UseParallel) 
                                sortingProc =
            let rewrap (id,s) = 
                let su, sc = sortingProc s sortableSet
                su, sc
                //id, (SortingResultsW.makeStandard s sorterId sortableSetId su sc)

            match UseParallel.value(_parallel) with
            | true  -> sorterSet.sorters |> Map.toArray 
                                         |> Array.Parallel.map(fun s-> rewrap s)
            | false -> sorterSet.sorters |> Map.toArray 
                                         |> Array.map(fun s-> rewrap s)


        //let GetResults (sortableSet:SortableSetRollout) (_parallel:UseParallel)
        //               (sorterSet:SorterSet) (sorterId:Guid) (sortableSetId:Guid)
        //               (sortingStrategy:SortingStrategy) =
        //    match sortingStrategy with
        //    | AllSwitches -> fullRollout |> makeResults sortableSet sorterSet 
        //                                          sorterId sortableSetId _parallel
        //    | QuitWhenSorted -> checkRollout |> makeResults sortableSet sorterSet 
        //                                            sorterId sortableSetId _parallel
         

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

