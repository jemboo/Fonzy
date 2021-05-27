namespace global
open System
open SortingEval

module SortingOps =
    // uses a (sorter.switchcount * sortableCount ) length 
    // array to store each switch use, thus no SAG (Switch 
    // Action Grouping)
    let private evalSorterOnSortableWithNoSAG 
                (sorter:Sorter) 
                (mindex:int) (maxdex:int) 
                (intSetsRoll:IntSetsRollout) 
                (useTrack:int[])
                (sortableIndex:int) =
        let mutable looP = true
        let mutable localSwitchOffset = mindex
        let sortableSetRolloutOffset = sortableIndex * (Degree.value sorter.degree)
        let switchEventRolloutOffset = sortableIndex * (SwitchCount.value sorter.switchCount)
        while ((localSwitchOffset < maxdex) && looP) do
            let switch = sorter.switches.[localSwitchOffset]
            let lv = intSetsRoll.baseArray.[switch.low + sortableSetRolloutOffset]
            let hv = intSetsRoll.baseArray.[switch.hi + sortableSetRolloutOffset]
            if(lv > hv) then
                intSetsRoll.baseArray.[switch.hi + sortableSetRolloutOffset] <- lv
                intSetsRoll.baseArray.[switch.low + sortableSetRolloutOffset] <- hv
                useTrack.[localSwitchOffset + switchEventRolloutOffset] <- 1
            looP <- ((localSwitchOffset % 20 > 0) ||
                     (not (Combinatorics.isSortedOffset 
                                            intSetsRoll.baseArray 
                                            sortableSetRolloutOffset 
                                            (Degree.value(intSetsRoll.degree)))))
            localSwitchOffset <- localSwitchOffset + 1


    // creates a (sorter.switchcount * sortableCount ) length 
    // array to store each switch use, thus no SAG (Switch 
    // Action Grouping)
    let evalNoGrouping 
                        (sorter:Sorter) 
                        (intSetsRollout:IntSetsRollout) 
                        (switchusePlan:Sorting.SwitchUsePlan) =
        let switchCount = (SwitchCount.value sorter.switchCount)
        let firstSwitchDex, lastSwitchDex = 
            match switchusePlan with
            | Sorting.SwitchUsePlan.All -> (0, switchCount)
            | Sorting.SwitchUsePlan.Range (min, max) -> (min, max)
        let ssRollCopy = IntSetsRollout.copy intSetsRollout
        let seRoll = SwitchEventRolloutInt.create
                                            sorter.switchCount
                                            intSetsRollout.sortableCount
        let mutable sortableIndex=0
        while (sortableIndex < (SortableCount.value intSetsRollout.sortableCount)) do
                evalSorterOnSortableWithNoSAG 
                        sorter firstSwitchDex 
                        lastSwitchDex ssRollCopy 
                        seRoll.useRoll.values sortableIndex
                sortableIndex <- sortableIndex + 1
        SwitchEventRecords.NoGrouping {
            NoGrouping.switchEventRollout = seRoll; 
            NoGrouping.sortableRollout = SortableRollout.Int 
                                                ssRollCopy
        }


    // uses a sorter.switchcount length array to store accumulated
    // switch uses
    let private evalSorterOnSortableSAGbySwitch 
                    (sorter:Sorter) 
                    (mindex:int) (maxdex:int) 
                    (switchUses:SwitchUses) 
                    (sortableSetRollout:IntSetsRollout) 
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


    // uses a sorter.switchcount length array to store accumulated
    // switch uses
    let evalGroupBySwitch 
                    (sorter:Sorter) 
                    (ssRollout:IntSetsRollout) 
                    (switchusePlan:Sorting.SwitchUsePlan) =
        let switchCount = (SwitchCount.value sorter.switchCount)
        let firstSwitchDex, lastSwitchDex = 
            match switchusePlan with
            | Sorting.SwitchUsePlan.All -> (0, switchCount)
            | Sorting.SwitchUsePlan.Range (min, max) -> (min, max)
        let switchUses = SwitchUses.createEmpty sorter.switchCount
        let sortableSetRolloutCopy = (IntSetsRollout.copy ssRollout)
        let mutable sortableIndex=0
        while (sortableIndex < (SortableCount.value ssRollout.sortableCount)) do
                evalSorterOnSortableSAGbySwitch 
                    sorter firstSwitchDex lastSwitchDex 
                    switchUses sortableSetRolloutCopy sortableIndex
                sortableIndex <- sortableIndex + 1
        SwitchEventRecords.BySwitch {
            GroupBySwitch.switchUses = switchUses; 
            GroupBySwitch.sortableRollout = SortableRollout.Int 
                                                sortableSetRolloutCopy
        }
        
    // creates a sorter.switchcount length array to store accumulated
    // sortable uses
    let private evalSwitchesGroupBySortable 
                (sorter:Sorter) 
                (mindex:int) (maxdex:int) 
                (sortableUses:SortableUses) 
                (sortableSetRollout:IntSetsRollout) 
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
    //// sortable uses
    let evalGroupBySortable 
                    (sorter:Sorter) 
                    (sortableSetRollout:IntSetsRollout) 
                    (switchusePlan:Sorting.SwitchUsePlan) =
        let switchCount = (SwitchCount.value sorter.switchCount)
        let firstSwitchDex, lastSwitchDex = 
            match switchusePlan with
            | Sorting.SwitchUsePlan.All -> (0, switchCount)
            | Sorting.SwitchUsePlan.Range (min, max) -> (min, max)
        let sortableUses = SortableUses.createEmpty sortableSetRollout.sortableCount

        let intSetsRolloutCopy = (IntSetsRollout.copy sortableSetRollout)
        let mutable sortableIndex = 0
        while (sortableIndex < (SortableCount.value sortableSetRollout.sortableCount)) do
                evalSwitchesGroupBySortable 
                    sorter firstSwitchDex lastSwitchDex sortableUses 
                    intSetsRolloutCopy sortableIndex
                sortableIndex <- sortableIndex + 1
        SwitchEventRecords.BySortable   {
            GroupBySortable.sortableUses = sortableUses; 
            GroupBySortable.sortableRollout = SortableRollout.Int 
                                                    intSetsRolloutCopy
        }


    let evalSorterOnSortableSetRollout 
                    (sorter:Sorter)
                    (sortableSetRollout:IntSetsRollout)
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
                   (sortableSet:SortableSetBinary)
                   (switchusePlan:Sorting.SwitchUsePlan) 
                   (switchEventAgg:Sorting.EventGrouping) =
        let sortableSetRollout = 
            sortableSet.sortables
                |> IntSetsRollout.fromIntBits
                        sorter.degree
                |> Result.ExtractOrThrow
        evalSorterOnSortableSetRollout
            sorter sortableSetRollout switchusePlan switchEventAgg



    module SorterSet =

        let eval<'T> 
                 (sorterSet:SorterSet)
                 (sortableSet:SortableSetBinary)
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
                let! ssRoll = sortableSet.sortables 
                              |> IntSetsRollout.fromIntBits
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
            (sortableSet:SortableSetBinary)
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
                             (testCase:IntBits) =
            let mutable i = 0
            let mutable lstRet = [testCase]
            let mutable newCase = testCase

            while (i < switches.Length) do
                newCase <- newCase |> IntBits.copy
                let intArray = newCase.values
                let switch = switches.[i]
                let lv = intArray.[switch.low]
                let hv = intArray.[switch.hi]
                if(lv > hv) then
                    intArray.[switch.hi] <- lv
                    intArray.[switch.low] <- hv
                lstRet <- newCase::lstRet
                i <- i+1
            lstRet |> List.rev


        let sortTHistSwitchList (sorter:Sorter) 
                                (mindex:int) 
                                (maxdex:int) 
                                (testCase:IntBits) =
            let sws = sorter.switches |> Array.skip(mindex)
                                      |> Array.take(maxdex - mindex)
                                      |> Array.toList
            sortTHistSwitches sws testCase


        let sortTHist (sorter:Sorter) (testCase:IntBits) =
            let sl = SwitchCount.value sorter.switchCount
            sortTHistSwitchList sorter 0 (sl - 1) testCase

