namespace global
open System


module SortingOps =
    // creates a (sorter.switchcount * sortableCount ) length 
    // array to store each switch use, thus no SAG (Switch 
    // Action Grouping)
    let private EvalSorterOnSortableWithNoSAG 
                (sorter:Sorter) 
                (mindex:int) (maxdex:int) 
                (sortableSetRollout:SortableSetRollout) 
                (useTrack:int[])
                (sortableIndex:int) =
        let mutable looP = true
        let mutable localSwitchOffset = mindex
        let sortableSetRolloutOffset = sortableIndex * (Degree.value sorter.degree)
        let switchEventRolloutOffset = sortableIndex * (SwitchCount.value sorter.switchCount)
        while ((localSwitchOffset < maxdex) && looP) do
            let switch = sorter.switches.[localSwitchOffset]
            let lv = sortableSetRollout.baseArray.[switch.low + sortableSetRolloutOffset]
            let hv = sortableSetRollout.baseArray.[switch.hi + sortableSetRolloutOffset]
            if(lv > hv) then
                sortableSetRollout.baseArray.[switch.hi + sortableSetRolloutOffset] <- lv
                sortableSetRollout.baseArray.[switch.low + sortableSetRolloutOffset] <- hv
                useTrack.[localSwitchOffset + switchEventRolloutOffset] <- 1
            looP <- ((localSwitchOffset % 20 > 0) ||
                     (not (Combinatorics.isSortedOffset 
                                            sortableSetRollout.baseArray 
                                            sortableSetRolloutOffset 
                                            (Degree.value(sortableSetRollout.degree)))))
            localSwitchOffset <- localSwitchOffset + 1


    // creates a (sorter.switchcount * sortableCount ) length 
    // array to store each switch use, thus no SAG (Switch 
    // Action Grouping)
    let evalNoGrouping 
                            (sorter:Sorter) 
                            (sortableSetRollout:SortableSetRollout) 
                            (switchusePlan:Sorting.SwitchUsePlan) =
        let switchCount = (SwitchCount.value sorter.switchCount)
        let firstSwitchDex, lastSwitchDex = 
            match switchusePlan with
            | Sorting.SwitchUsePlan.All -> (0, switchCount)
            | Sorting.SwitchUsePlan.Range (min, max) -> (min, max)
        let sortableSetRolloutCopy = (SortableSetRollout.copy sortableSetRollout)
        let switchEventRollout = SwitchEventRollout.create sorter.switchCount sortableSetRollout.sortableCount
        let mutable sortableIndex=0
        while (sortableIndex < (SortableCount.value sortableSetRollout.sortableCount)) do
                EvalSorterOnSortableWithNoSAG 
                        sorter firstSwitchDex 
                        lastSwitchDex sortableSetRolloutCopy 
                        switchEventRollout.useRoll sortableIndex
                sortableIndex <- sortableIndex + 1
        SortingEval.SwitchEventRecords.NoGrouping {
            SortingEval.NoGrouping.switchEventRollout = switchEventRollout; 
            SortingEval.NoGrouping.sortableSetRollout = sortableSetRolloutCopy
        }


    // creates a sorter.switchcount length array to store accumulated
    // switch uses
    let private EvalSorterOnSortableSAGbySwitch 
                    (sorter:Sorter) 
                    (mindex:int) (maxdex:int) 
                    (switchUses:SwitchUses) 
                    (sortableSetRollout:SortableSetRollout) 
                    (sortableIndex:int) =
        let useWeights = (SwitchUses.getWeights switchUses)
        let sortableSetRolloutOffset = sortableIndex * (Degree.value sorter.degree)
        let mutable looP = true
        let mutable localSwitchOffset = mindex
        while ((localSwitchOffset < maxdex) && looP) do
            let switch = sorter.switches.[localSwitchOffset]
            let lv = sortableSetRollout.baseArray.[switch.low + sortableSetRolloutOffset]
            let hv = sortableSetRollout.baseArray.[switch.hi + sortableSetRolloutOffset]
            if(lv > hv) then
                sortableSetRollout.baseArray.[switch.hi + sortableSetRolloutOffset] <- lv
                sortableSetRollout.baseArray.[switch.low + sortableSetRolloutOffset] <- hv
                useWeights.[localSwitchOffset] <- useWeights.[localSwitchOffset] + 1
                looP <- ((localSwitchOffset % 20 > 0) ||
                         (not (Combinatorics.isSortedOffset 
                                                sortableSetRollout.baseArray 
                                                sortableSetRolloutOffset 
                                                (Degree.value(sorter.degree)))))
            localSwitchOffset <- localSwitchOffset+1

    // creates a sorter.switchcount length array to store accumulated
    // switch uses
    let evalGroupBySwitch 
                    (sorter:Sorter) 
                    (ssRollout:SortableSetRollout) 
                    (switchusePlan:Sorting.SwitchUsePlan) =
        let switchCount = (SwitchCount.value sorter.switchCount)
        let firstSwitchDex, lastSwitchDex = 
            match switchusePlan with
            | Sorting.SwitchUsePlan.All -> (0, switchCount)
            | Sorting.SwitchUsePlan.Range (min, max) -> (min, max)
        let switchUses = SwitchUses.createEmpty sorter.switchCount
        let sortableSetRolloutCopy = (SortableSetRollout.copy ssRollout)
        let mutable sortableIndex=0
        while (sortableIndex < (SortableCount.value ssRollout.sortableCount)) do
                EvalSorterOnSortableSAGbySwitch 
                    sorter firstSwitchDex lastSwitchDex 
                    switchUses sortableSetRolloutCopy sortableIndex
                sortableIndex <- sortableIndex + 1
        SortingEval.SwitchEventRecords.BySwitch {
            SortingEval.GroupBySwitch.switchUses = switchUses; 
            SortingEval.GroupBySwitch.sortableSetRollout = sortableSetRolloutCopy
        }
        
    // creates a sorter.switchcount length array to store accumulated
    // switch uses
    let private evalSwitchesGroupBySortable 
                (sorter:Sorter) 
                (mindex:int) (maxdex:int) 
                (sortableUses:SortableUses) 
                (sortableSetRollout:SortableSetRollout) 
                (sortableIndex:int) =
        let useWeights = SortableUses.getWeights sortableUses
        let sortableSetRolloutOffset = sortableIndex * (Degree.value sorter.degree)
        let mutable looP = true
        let mutable localSwitchOffset = mindex
        while ((localSwitchOffset < maxdex) && looP) do
            let switch = sorter.switches.[localSwitchOffset]
            let lv = sortableSetRollout.baseArray.[switch.low + sortableSetRolloutOffset]
            let hv = sortableSetRollout.baseArray.[switch.hi + sortableSetRolloutOffset]
            if(lv > hv) then
                sortableSetRollout.baseArray.[switch.hi + sortableSetRolloutOffset] <- lv
                sortableSetRollout.baseArray.[switch.low + sortableSetRolloutOffset] <- hv
                useWeights.[sortableIndex] <- useWeights.[sortableIndex] + 1
                looP <- ((localSwitchOffset % 20 > 0) ||
                         (not (Combinatorics.isSortedOffset 
                                                sortableSetRollout.baseArray 
                                                sortableSetRolloutOffset 
                                                (Degree.value(sorter.degree)))))
            localSwitchOffset <- localSwitchOffset+1


    //// creates a sorter.switchcount length array to store accumulated
    //// switch uses
    let evalGroupBySortable 
                    (sorter:Sorter) 
                    (sortableSetRollout:SortableSetRollout) 
                    (switchusePlan:Sorting.SwitchUsePlan) =
        let switchCount = (SwitchCount.value sorter.switchCount)
        let firstSwitchDex, lastSwitchDex = 
            match switchusePlan with
            | Sorting.SwitchUsePlan.All -> (0, switchCount)
            | Sorting.SwitchUsePlan.Range (min, max) -> (min, max)
        let sortableUses = SortableUses.createEmpty sortableSetRollout.sortableCount

        let sortableSetRolloutCopy = (SortableSetRollout.copy sortableSetRollout)
        let mutable sortableIndex = 0
        while (sortableIndex < (SortableCount.value sortableSetRollout.sortableCount)) do
                evalSwitchesGroupBySortable 
                    sorter firstSwitchDex lastSwitchDex sortableUses 
                    sortableSetRolloutCopy sortableIndex
                sortableIndex <- sortableIndex + 1
        SortingEval.SwitchEventRecords.BySortable   {
            SortingEval.GroupBySortable.sortableUses = sortableUses; 
            SortingEval.GroupBySortable.sortableSetRollout = sortableSetRolloutCopy
        }

    let evalSorterOnSortableSetRollout (sorter:Sorter)
                    (sortableSetRollout:SortableSetRollout)
                    (switchusePlan:Sorting.SwitchUsePlan) 
                    (switchEventAgg:Sorting.EventGrouping) =
        match switchEventAgg with
        | Sorting.EventGrouping.NoGrouping -> 
                evalNoGrouping 
                    sorter sortableSetRollout switchusePlan
        | Sorting.EventGrouping.BySwitch -> 
                evalGroupBySwitch 
                    sorter sortableSetRollout switchusePlan
        | Sorting.EventGrouping.BySortable -> 
                evalGroupBySortable 
                    sorter sortableSetRollout switchusePlan


    let evalSorter (sorter:Sorter)
                   (sortableSet:SortableSetExplicit)
                   (switchusePlan:Sorting.SwitchUsePlan) 
                   (switchEventAgg:Sorting.EventGrouping) =
        let sortableSetRollout = 
            sortableSet.sortableIntArrays
                |> SortableSetRollout.fromSortableIntArrays
                        sorter.degree
                |> Result.ExtractOrThrow
        evalSorterOnSortableSetRollout
            sorter sortableSetRollout switchusePlan switchEventAgg


    module SorterSet =
        let eval (sorterSet:SorterSet)
                 (sortableSet:SortableSetExplicit)
                 (switchusePlan:Sorting.SwitchUsePlan) 
                 (switchEventAgg:Sorting.EventGrouping) 
                 (_parallel:UseParallel) =
            let sortableSetRollout = sortableSet.sortableIntArrays 
                                        |> SortableSetRollout.fromSortableIntArrays
                                                sorterSet.degree
                                        |> Result.ExtractOrThrow
            let rewrap (id, s) = 
                let res = evalSorterOnSortableSetRollout 
                            s sortableSetRollout switchusePlan switchEventAgg
                res
                //id, (SortingResultsW.makeStandard s sorterId sortableSetId su sc)

            match UseParallel.value(_parallel) with
            | true  -> sorterSet.sorters |> Map.toArray 
                                         |> Array.Parallel.map(fun s-> rewrap s)
            | false -> sorterSet.sorters |> Map.toArray 
                                         |> Array.map(fun s-> rewrap s)


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

        let switchOnSortableIntArray (switch:Switch) 
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


        let rec sortableHistory (swtiches:Switch list)
                                (sortHistory:SortableIntArray list) =
            match swtiches with
            | [] -> sortHistory
            | swHead::swTail -> [(switchOnSortableIntArray swHead (sortHistory|> List.last))] |> List.append
                                    (sortableHistory swTail sortHistory)


        let switchesOnSortableIntArray (swtiches:Switch list)
                                       (sortableIntArray:SortableIntArray) =
            let recList = (swtiches  |> List.rev)
            (sortableHistory swtiches  [sortableIntArray])


        let sortTHistSection2 (sorter:Sorter) (mindex:int) (maxdex:int)
                            (testCase:SortableIntArray) =
            let sws = sorter.switches |> Array.skip(mindex)
                                        |> Array.take(maxdex - mindex)
                                        |> Array.toList
            switchesOnSortableIntArray sws testCase


        let sortTHist2 (sorter:Sorter) (testCase:SortableIntArray) =
            let sl = SwitchCount.value sorter.switchCount
            sortTHistSection2 sorter 0 (sl - 1) testCase


        let sortTHistSwitches(switches:Switch list)
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


        let sortTHistSwitchList (sorter:Sorter) (mindex:int) (maxdex:int) 
                                (testCase:SortableIntArray) =
            let sws = sorter.switches |> Array.skip(mindex)
                                        |> Array.take(maxdex - mindex)
                                        |> Array.toList
            sortTHistSwitches sws testCase


        let sortTHist (sorter:Sorter) (testCase:SortableIntArray) =
            let sl = SwitchCount.value sorter.switchCount
            sortTHistSwitchList sorter 0 (sl - 1) testCase

