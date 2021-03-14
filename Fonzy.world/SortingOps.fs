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
    let EvalSorterOnSortableSetWithNoSAG 
                            (sorter:Sorter) 
                            (sortableSetRollout:SortableSetRollout) 
                            (switchusePlan:SortingEval.SwitchUsePlan) =
        let switchCount = (SwitchCount.value sorter.switchCount)
        let firstSwitchDex, lastSwitchDex = 
            match switchusePlan with
            | SortingEval.SwitchUsePlan.All -> (0, switchCount)
            | SortingEval.SwitchUsePlan.Range (min, max) -> (min, max)
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
            SortingEval.SeNoGrouping.switchEventRollout = switchEventRollout; 
            SortingEval.SeNoGrouping.sortableSetRollout = sortableSetRolloutCopy
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
    let EvalSorterOnSortableSetSAGbySwitch 
                    (sorter:Sorter) 
                    (ssRollout:SortableSetRollout) 
                    (switchusePlan:SortingEval.SwitchUsePlan) =
        let switchCount = (SwitchCount.value sorter.switchCount)
        let firstSwitchDex, lastSwitchDex = 
            match switchusePlan with
            | SortingEval.SwitchUsePlan.All -> (0, switchCount)
            | SortingEval.SwitchUsePlan.Range (min, max) -> (min, max)
        let switchUses = SwitchUses.createEmpty sorter.switchCount
        let sortableSetRolloutCopy = (SortableSetRollout.copy ssRollout)
        let mutable sortableIndex=0
        while (sortableIndex < (SortableCount.value ssRollout.sortableCount)) do
                EvalSorterOnSortableSAGbySwitch 
                    sorter firstSwitchDex lastSwitchDex 
                    switchUses sortableSetRolloutCopy sortableIndex
                sortableIndex <- sortableIndex + 1
        SortingEval.SwitchEventRecords.GroupbySwitch {
            SortingEval.SeGroupbySwitch.switchUses = switchUses; 
            SortingEval.SeGroupbySwitch.sortableSetRollout = sortableSetRolloutCopy
        }
        
    // creates a sorter.switchcount length array to store accumulated
    // switch uses
    let private EvalSorterOnSortableSAGbySortable 
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
    let EvalSorterOnSortableSetSAGbySortable 
                    (sorter:Sorter) 
                    (sortableSetRollout:SortableSetRollout) 
                    (switchusePlan:SortingEval.SwitchUsePlan) =
        let switchCount = (SwitchCount.value sorter.switchCount)
        let firstSwitchDex, lastSwitchDex = 
            match switchusePlan with
            | SortingEval.SwitchUsePlan.All -> (0, switchCount)
            | SortingEval.SwitchUsePlan.Range (min, max) -> (min, max)
        let sortableUses = SortableUses.createEmpty sortableSetRollout.sortableCount

        let sortableSetRolloutCopy = (SortableSetRollout.copy sortableSetRollout)
        let mutable sortableIndex = 0
        while (sortableIndex < (SortableCount.value sortableSetRollout.sortableCount)) do
                EvalSorterOnSortableSAGbySortable 
                    sorter firstSwitchDex lastSwitchDex sortableUses 
                    sortableSetRolloutCopy sortableIndex
                sortableIndex <- sortableIndex + 1
        SortingEval.SwitchEventRecords.GroupBySortable   {
            SortingEval.SeGroupBySortable.sortableUses = sortableUses; 
            SortingEval.SeGroupBySortable.sortableSetRollout = sortableSetRolloutCopy
        }

    let evalSorterOnSortableSetRollout (sorter:Sorter)
                    (sortableSetRollout:SortableSetRollout)
                    (switchusePlan:SortingEval.SwitchUsePlan) 
                    (switchEventAgg:SortingEval.SwitchEventGrouping) =

        match switchEventAgg with
        | SortingEval.SwitchEventGrouping.NoGrouping -> 
                EvalSorterOnSortableSetWithNoSAG 
                    sorter sortableSetRollout switchusePlan
        | SortingEval.SwitchEventGrouping.BySwitch -> 
                EvalSorterOnSortableSetSAGbySwitch 
                    sorter sortableSetRollout switchusePlan
        | SortingEval.SwitchEventGrouping.BySortable -> 
                EvalSorterOnSortableSetSAGbySortable 
                    sorter sortableSetRollout switchusePlan


    let evalSorter (sorter:Sorter)
                   (sortableSet:SortableSetExplicit)
                   (switchusePlan:SortingEval.SwitchUsePlan) 
                   (switchEventAgg:SortingEval.SwitchEventGrouping) =
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
                 (switchusePlan:SortingEval.SwitchUsePlan) 
                 (switchEventAgg:SortingEval.SwitchEventGrouping) 
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

