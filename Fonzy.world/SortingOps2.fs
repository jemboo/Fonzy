namespace global
open System
open SortingEval

module SortingOps2 =

    let private EvalSorterOnSbpWithNoSAG 
                (sorter:Sorter) 
                (mindex:int) (maxdex:int) 
                (sortableSetRollout:SortableSetRollout) 
                (useTrack:int[])
                (sortableIndex:int) =
        let mutable localSwitchOffset = mindex
        let sortableSetRolloutOffset = sortableIndex * (Degree.value sorter.degree)
        let switchEventRolloutOffset = sortableIndex * (SwitchCount.value sorter.switchCount)
        while (localSwitchOffset < maxdex) do
            let switch = sorter.switches.[localSwitchOffset]
            let lv = sortableSetRollout.baseArray.[switch.low + sortableSetRolloutOffset]
            let hv = sortableSetRollout.baseArray.[switch.hi + sortableSetRolloutOffset]
            let rv = useTrack.[localSwitchOffset + switchEventRolloutOffset]
            sortableSetRollout.baseArray.[switch.hi + sortableSetRolloutOffset] <- (lv ||| hv)
            sortableSetRollout.baseArray.[switch.low + sortableSetRolloutOffset] <- (lv &&& hv)
            useTrack.[localSwitchOffset + switchEventRolloutOffset] <- (((~~~hv) &&& lv) ||| rv)
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
                EvalSorterOnSbpWithNoSAG 
                        sorter firstSwitchDex 
                        lastSwitchDex sortableSetRolloutCopy 
                        switchEventRollout.useRoll sortableIndex
                sortableIndex <- sortableIndex + 1
        SwitchEventRecords.NoGrouping {
            NoGrouping.switchEventRollout = switchEventRollout; 
            NoGrouping.sortableSetRollout = sortableSetRolloutCopy
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
        SwitchEventRecords.BySwitch {
            GroupBySwitch.switchUses = switchUses; 
            GroupBySwitch.sortableSetRollout = sortableSetRolloutCopy
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
        SwitchEventRecords.BySortable   {
            GroupBySortable.sortableUses = sortableUses; 
            GroupBySortable.sortableSetRollout = sortableSetRolloutCopy
        }

    let evalSorterOnSortableSetRollout 
                    (sorter:Sorter)
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
        let eval<'T> 
                 (sorterSet:SorterSet)
                 (sortableSet:SortableSetExplicit)
                 (switchusePlan:Sorting.SwitchUsePlan) 
                 (switchEventAgg:Sorting.EventGrouping) 
                 (_parallel:UseParallel) 
                 (proc:ResultOfSorterOnSortableSet -> Result<'T, string>) =

            let rewrap tup ssr = 
                let sorterId, sorter = tup
                let swEvRecs = evalSorterOnSortableSetRollout 
                                    sorter ssr switchusePlan switchEventAgg
                let resSoSS = {
                    ResultOfSorterOnSortableSet.sorter = sorter;
                    ResultOfSorterOnSortableSet.switchEventRecords = swEvRecs;
                    ResultOfSorterOnSortableSet.sorterId = sorterId;
                    ResultOfSorterOnSortableSet.sortableSetId = sortableSet.id
                }
                proc resSoSS

            result  {
                let! ssRoll = sortableSet.sortableIntArrays 
                              |> SortableSetRollout.fromSortableIntArrays
                                    sorterSet.degree
                return!
                    match UseParallel.value(_parallel) with
                    | true  -> sorterSet.sorters |> Map.toArray 
                                                 |> Array.Parallel.map(fun s-> rewrap s ssRoll)
                                                 |> Array.toList
                                                 |> Result.sequence
                    | false -> sorterSet.sorters |> Map.toList 
                                                 |> List.map(fun s-> rewrap s ssRoll)
                                                 |> Result.sequence
            }

        let getSorterPerfBins 
            (sorterSet:SorterSet)
            (sortableSet:SortableSetExplicit)
            (switchusePlan:Sorting.SwitchUsePlan)
            (_parallel:UseParallel) =
            result {
                let! sorterEffs = 
                        eval 
                            sorterSet 
                            sortableSet 
                            switchusePlan
                            Sorting.EventGrouping.BySwitch
                            _parallel
                            SortingEval.SortingRecords.getSorterEff

                let bins = sorterEffs 
                                |> SorterPerfBin.fromSorterEffs

                return bins
            }


    module History =

        let sortTHistSwitches(switches:Switch list)
                             (pBits:bitsP32) =
            let mutable i = 0
            let mutable lstRet = [pBits]
            let mutable newCase = pBits

            while (i < switches.Length) do
                newCase <- newCase |> bitsP32.copy
                let uintArray = newCase.values
                let sw = switches.[i]
                let lv = uintArray.[sw.low]
                let hv = uintArray.[sw.hi]
                let nhv = (lv ||| hv)
                let nlv = (lv &&& hv)
                uintArray.[sw.hi] <- (lv ||| hv)
                uintArray.[sw.low] <- (lv &&& hv)
                lstRet <- newCase::lstRet
                i <- i+1
            lstRet |> List.rev


        let sortTHistSwitchList (sorter:Sorter) 
                                 (mindex:int) 
                                 (maxdex:int) 
                                 (pBits:bitsP32) =
            let sws = sorter.switches |> Array.skip(mindex)
                                      |> Array.take(maxdex - mindex)
                                      |> Array.toList
            sortTHistSwitches sws pBits


        let sortTHist2 (sorter:Sorter) 
                       (pBits:bitsP32) =
            let sl = SwitchCount.value sorter.switchCount
            sortTHistSwitchList sorter 0 (sl - 1) pBits

