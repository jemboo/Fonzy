﻿namespace global
open System
open SortingEval

module SortingInts =
    // uses a (sorter.switchcount * sortableCount ) length 
    // array to store each switch use, thus no SAG (Switch 
    // Action Grouping)
    let private switchRangeWithNoSAG 
                (sorter:Sorter) 
                (mindex:int) 
                (maxdex:int) 
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
    let sorterWithNoSAG 
                    (sorter:Sorter) 
                    (intSetsRollout:IntSetsRollout) 
                    (switchusePlan:Sorting.SwitchUsePlan) =
        let switchCount = (SwitchCount.value sorter.switchCount)

        let emptyRollout () = 
            SwitchEventRolloutInt.create
                            sorter.switchCount
                            intSetsRollout.sortableCount

        let switchPlanRollout (weights:int[]) = 
            SwitchEventRolloutInt.init
                            weights
                            intSetsRollout.sortableCount
                            
        let firstSwitchDex, lastSwitchDex, seRollout = 
            match switchusePlan with
            | Sorting.SwitchUsePlan.All -> 
                (0, switchCount, emptyRollout())
            | Sorting.SwitchUsePlan.Range (min, max) -> 
                (min, max, emptyRollout())
            | Sorting.SwitchUsePlan.Indexes (min, max, weights) -> 
                (min, max, switchPlanRollout weights)
        let ssRollCopy = IntSetsRollout.copy intSetsRollout

        let mutable sortableIndex=0
        while (sortableIndex < (SortableCount.value intSetsRollout.sortableCount)) do
                switchRangeWithNoSAG 
                        sorter 
                        firstSwitchDex 
                        lastSwitchDex 
                        ssRollCopy 
                        seRollout.useRoll.values 
                        sortableIndex

                sortableIndex <- sortableIndex + 1
        switchEventRecords.NoGrouping {
            noGrouping.switchEventRollout = seRollout |> switchEventRollout.Int
            noGrouping.sortableRollout = ssRollCopy |> sortableSetRollout.Int 
                                                
        }


    // uses a sorter.switchcount length array to store accumulated
    // switch uses
    let private switchRangeMakeSwitchUses 
                    (sorter:Sorter) 
                    (mindex:int) 
                    (maxdex:int) 
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
    let sorterMakeSwitchUses 
                    (sorter:Sorter) 
                    (ssRollout:IntSetsRollout) 
                    (switchusePlan:Sorting.SwitchUsePlan) =
        let switchCount = (SwitchCount.value sorter.switchCount)
        let firstSwitchDex, lastSwitchDex, switchUses = 
            match switchusePlan with
            | Sorting.SwitchUsePlan.All -> 
                (0, switchCount, (SwitchUses.createEmpty sorter.switchCount))
            | Sorting.SwitchUsePlan.Range (min, max) -> 
                (min, max, (SwitchUses.createEmpty sorter.switchCount))
            | Sorting.SwitchUsePlan.Indexes (min, max, wgts) -> 
                let cpyWgts = wgts |> Array.copy
                (min, max, SwitchUses.init cpyWgts)

        let sortableSetRolloutCopy = (IntSetsRollout.copy ssRollout)
        let mutable sortableIndex=0
        while (sortableIndex < (SortableCount.value ssRollout.sortableCount)) do
                switchRangeMakeSwitchUses 
                    sorter 
                    firstSwitchDex 
                    lastSwitchDex 
                    switchUses 
                    sortableSetRolloutCopy 
                    sortableIndex
                sortableIndex <- sortableIndex + 1
        switchEventRecords.BySwitch {
            groupBySwitch.switchUses = switchUses; 
            groupBySwitch.sortableRollout = sortableSetRollout.Int 
                                                sortableSetRolloutCopy
        }
        
    let evalSorterOnIntSetsRollout
                    (sorter:Sorter)
                    (sortableSetRollout:IntSetsRollout)
                    (switchusePlan:Sorting.SwitchUsePlan) 
                    (switchEventAgg:Sorting.EventGrouping) =
        match switchEventAgg with
        | Sorting.EventGrouping.NoGrouping -> 
                sorterWithNoSAG 
                    sorter sortableSetRollout switchusePlan
        | Sorting.EventGrouping.BySwitch -> 
                sorterMakeSwitchUses 
                    sorter sortableSetRollout switchusePlan


    let evalSorterOnBinary (sorter:Sorter)
                   (sortableSet:SortableSetBinary)
                   (switchusePlan:Sorting.SwitchUsePlan) 
                   (switchEventAgg:Sorting.EventGrouping) =
        let sortableSetRollout = 
            sortableSet.sortables
                |> IntSetsRollout.fromIntBits
                        sorter.degree
                |> Result.ExtractOrThrow
        evalSorterOnIntSetsRollout
            sorter sortableSetRollout switchusePlan switchEventAgg


    let evalSorterOnInteger (sorter:Sorter)
                            (sortableSet:SortableSetInteger)
                            (switchusePlan:Sorting.SwitchUsePlan) 
                            (switchEventAgg:Sorting.EventGrouping) =
        let sortableSetRollout = 
            sortableSet.sortables
                |> IntSetsRollout.fromIntArrays
                                    sorter.degree
                |> Result.ExtractOrThrow
        evalSorterOnIntSetsRollout
            sorter sortableSetRollout switchusePlan switchEventAgg



    module SorterSet =

        let eval<'T> 
                (sorterSet:SorterSet)
                (intSetsRollout:IntSetsRollout)
                (sortableSetId:SortableSetId)
                (switchusePlan:Sorting.SwitchUsePlan) 
                (switchEventAgg:Sorting.EventGrouping) 
                (_parallel:UseParallel) 
                (proc:sortingResult -> Result<'T, string>) =

            let rewrap tup ssr = 
                let sorterId, sorter = tup
                let swEvRecs = evalSorterOnIntSetsRollout 
                                    sorter ssr switchusePlan switchEventAgg
                let resSoSS = {
                    sortingResult.sorter = sorter;
                    sortingResult.switchEventRecords = swEvRecs;
                    sortingResult.sorterId = sorterId;
                    sortingResult.sortableSetId = sortableSetId
                }
                proc resSoSS

            result  {
                return!
                    match UseParallel.value(_parallel) with
                    | true  -> sorterSet.sorters |> Map.toArray 
                                                 |> Array.Parallel.map(fun s-> rewrap s intSetsRollout)
                                                 |> Array.toList
                                                 |> Result.sequence
                    | false -> sorterSet.sorters |> Map.toList 
                                                 |> List.map(fun s-> rewrap s intSetsRollout)
                                                 |> Result.sequence
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
            sortTHistSwitchList sorter 0 sl testCase

