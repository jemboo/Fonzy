namespace global
open System
open SortingEval

module SortingInts =
    // uses a (sorter.switchcount * sortableCount ) length 
    // array to store each switch use, thus no SAG (Switch 
    // Action Grouping)
    let private switchRangeWithNoSAG 
                (sorter:sorter) 
                (mindex:int) 
                (maxdex:int) 
                (intSetsRoll:intSetsRollout) 
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
                    (sorter:sorter) 
                    (intSetsRollout:intSetsRollout) 
                    (switchusePlan:Sorting.switchUsePlan) =
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
            | Sorting.switchUsePlan.All -> 
                (0, switchCount, emptyRollout())
            | Sorting.switchUsePlan.Range (min, max) -> 
                (min, max, emptyRollout())
            | Sorting.switchUsePlan.Indexes (min, max, swu) -> 
                (min, max, switchPlanRollout swu.weights)
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
                    (sorter:sorter) 
                    (mindex:int) 
                    (maxdex:int) 
                    (switchUses:switchUses) 
                    (sortableSetRollout:intSetsRollout) 
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
                    (sorter:sorter) 
                    (ssRollout:intSetsRollout) 
                    (switchusePlan:Sorting.switchUsePlan) =
        let switchCount = (SwitchCount.value sorter.switchCount)
        let firstSwitchDex, lastSwitchDex, switchUses = 
            match switchusePlan with
            | Sorting.switchUsePlan.All -> 
                (0, switchCount, (SwitchUses.createEmpty sorter.switchCount))
            | Sorting.switchUsePlan.Range (min, max) -> 
                (min, max, (SwitchUses.createEmpty sorter.switchCount))
            | Sorting.switchUsePlan.Indexes (min, max, swu) -> 
                let cpyWgts = swu.weights |> Array.copy
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
                    (sorter:sorter)
                    (intSetsRollout:intSetsRollout)
                    (switchusePlan:Sorting.switchUsePlan) 
                    (switchEventAgg:Sorting.eventGrouping) =
        match switchEventAgg with
        | Sorting.eventGrouping.NoGrouping -> 
                sorterWithNoSAG 
                    sorter intSetsRollout switchusePlan
        | Sorting.eventGrouping.BySwitch -> 
                sorterMakeSwitchUses 
                    sorter intSetsRollout switchusePlan


    let evalSorterOnBinary (sorter:sorter)
                   (bitSet:bitSet[])
                   (switchusePlan:Sorting.switchUsePlan) 
                   (switchEventAgg:Sorting.eventGrouping) =
        let sortableSetRollout = 
            bitSet
                |> IntSetsRollout.fromBitSet
                        sorter.degree
                |> Result.ExtractOrThrow
        evalSorterOnIntSetsRollout
            sorter sortableSetRollout switchusePlan switchEventAgg


    let evalSorterOnInteger (sorter:sorter)
                            (intSets:intSet[])
                            (switchusePlan:Sorting.switchUsePlan) 
                            (switchEventAgg:Sorting.eventGrouping) =
        let sortableSetRollout = 
            intSets
                |> Array.map(fun a -> a.values)
                |> IntSetsRollout.fromIntArrays
                                    sorter.degree
                |> Result.ExtractOrThrow
        evalSorterOnIntSetsRollout
            sorter sortableSetRollout switchusePlan switchEventAgg



    module SorterSet =

        let eval<'T> 
                (sorterSet:sorterSet)
                (intSetsRollout:intSetsRollout)
                (switchusePlan:Sorting.switchUsePlan) 
                (switchEventAgg:Sorting.eventGrouping) 
                (_parallel:UseParallel) 
                (proc:sortingResult -> Result<'T, string>) =

            let rewrap ssr tup = 
                let sorterId, sorter = tup
                let swEvRecs = evalSorterOnIntSetsRollout 
                                    sorter ssr switchusePlan switchEventAgg
                let resSoSS = {
                    sortingResult.sorter = sorter;
                    sortingResult.switchEventRecords = swEvRecs;
                    sortingResult.sorterId = sorterId;
                }
                proc resSoSS

            result  {
                return!
                    match UseParallel.value(_parallel) with
                    | true  -> sorterSet.sorters |> Map.toArray 
                                                 |> Array.Parallel.map(rewrap intSetsRollout)
                                                 |> Array.toList
                                                 |> Result.sequence
                    | false -> sorterSet.sorters |> Map.toList 
                                                 |> List.map(rewrap intSetsRollout)
                                                 |> Result.sequence
            }



    module History =

        let sortTHistSwitches(switches:Switch list)
                             (testCase:bitSet) =
            let mutable i = 0
            let mutable lstRet = [testCase]
            let mutable newCase = testCase

            while (i < switches.Length) do
                newCase <- newCase |> BitSet.copy
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


        let sortTHistSwitchList (sorter:sorter) 
                                (mindex:int) 
                                (maxdex:int) 
                                (testCase:bitSet) =
            let sws = sorter.switches |> Array.skip(mindex)
                                      |> Array.take(maxdex - mindex)
                                      |> Array.toList
            sortTHistSwitches sws testCase


        let sortTHist (sorter:sorter) (testCase:bitSet) =
            let sl = SwitchCount.value sorter.switchCount
            sortTHistSwitchList sorter 0 sl testCase

