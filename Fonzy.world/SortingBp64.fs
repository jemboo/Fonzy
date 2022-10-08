namespace global
open System
open SortingEval

module SortingBp64 =

    let private switchRangeWithNoSAG 
                (sorter:sorter) 
                (mindex:int) 
                (maxdex:int) 
                (bp64SetsRollout:bP64SetsRollout) 
                (useTrack:uint64[])
                (bpBlockIndex:int) =

        let mutable localSwitchOffset = mindex
        let sortableSetRolloutOffset = bpBlockIndex * (Degree.value sorter.degree)
        let switchEventRolloutOffset = bpBlockIndex * 
                                       (SwitchCount.value sorter.switchCount)
        while (localSwitchOffset < maxdex) do
            let switch = sorter.switches.[localSwitchOffset]
            let lv = bp64SetsRollout.baseArray.[switch.low + sortableSetRolloutOffset]
            let hv = bp64SetsRollout.baseArray.[switch.hi + sortableSetRolloutOffset]
            bp64SetsRollout.baseArray.[switch.hi + sortableSetRolloutOffset] <- (lv ||| hv)
            bp64SetsRollout.baseArray.[switch.low + sortableSetRolloutOffset] <- (lv &&& hv)
            useTrack.[localSwitchOffset + switchEventRolloutOffset] <- ((~~~hv) &&& lv)
            localSwitchOffset <- localSwitchOffset + 1


    // creates a (sorter.switchcount * sortableCount ) length 
    // array to store each switch use, thus no SAG (Switch 
    // Action Grouping)
    let sorterWithNoSAG
                (sorter:sorter) 
                (bP64SetsRollout:bP64SetsRollout) 
                (switchusePlan:Sorting.switchUsePlan) =

        let switchCount = (SwitchCount.value sorter.switchCount)

        let emptyRollout () = 
            SwitchEventRolloutBp64.create
                sorter.switchCount
                bP64SetsRollout.sortableCount

        let switchPlanRollout (weights:int[]) = 
            SwitchEventRolloutBp64.init
                            bP64SetsRollout.sortableCount
                            weights
                            

        let firstSwitchDex, lastSwitchDex, seRollbp64 = 
            match switchusePlan with
            | Sorting.switchUsePlan.All -> 
                (0, switchCount, emptyRollout ())
            | Sorting.switchUsePlan.Range (min, max) -> 
                (min, max, emptyRollout ())
            | Sorting.switchUsePlan.Indexes (min, max, swu) -> 
                (min, max, switchPlanRollout swu.weights)
        
        let bPsRollCopy = BP64SetsRollout.copy bP64SetsRollout

        let mutable sortableBlockDex = 0
        while (sortableBlockDex < seRollbp64.sortableBlockCount) do
            switchRangeWithNoSAG
                            sorter
                            firstSwitchDex 
                            lastSwitchDex
                            bPsRollCopy
                            seRollbp64.useRoll.values
                            sortableBlockDex

            sortableBlockDex <- sortableBlockDex + 1

        switchEventRecords.NoGrouping {
            noGrouping.switchEventRollout = seRollbp64 |> switchEventRollout.Bp64
            noGrouping.sortableRollout = bPsRollCopy |> sortableSetRollout.Bp64                                       
        }


    // uses a sorter.switchcount length array to store accumulated
    // switch uses
    let private switchRangeMakeSwitchUses 
                    (sorter:sorter) 
                    (mindex:int) 
                    (maxdex:int) 
                    (switchUseB64:SwitchUseB64) 
                    (bp64SetsRollout:bP64SetsRollout) 
                    (sortableIndex:int) =
        let useWeights = (SwitchUseB64.getWeights switchUseB64)
        let mutable localSwitchOffset = mindex
        while (localSwitchOffset < maxdex) do
            let switch = sorter.switches.[localSwitchOffset]
            let lv = bp64SetsRollout.baseArray.[switch.low + sortableIndex]
            let hv = bp64SetsRollout.baseArray.[switch.hi + sortableIndex]
            let rv = useWeights.[localSwitchOffset]
            bp64SetsRollout.baseArray.[switch.hi + sortableIndex] <- (lv ||| hv)
            bp64SetsRollout.baseArray.[switch.low + sortableIndex] <- (lv &&& hv)
            useWeights.[localSwitchOffset] <- (((~~~hv) &&& lv) ||| rv)
            localSwitchOffset <- localSwitchOffset+1


    // creates a sorter.switchcount length array to store accumulated
    // switch uses
    let sorterMakeSwitchUses
                    (sorter:sorter) 
                    (bp64SetsRollout:bP64SetsRollout)  
                    (switchusePlan:Sorting.switchUsePlan) =

        let switchCount = (SwitchCount.value sorter.switchCount)
        let firstSwitchDex, lastSwitchDex, switchUseB64 =
            match switchusePlan with
            | Sorting.switchUsePlan.All -> 
                (0, switchCount, (SwitchUseB64.createEmpty sorter.switchCount))
            | Sorting.switchUsePlan.Range (min, max) -> 
                (min, max, (SwitchUseB64.createEmpty sorter.switchCount))
            | Sorting.switchUsePlan.Indexes (min, max, swu) -> 
                (min, max, (SwitchUseB64.init sorter.switchCount swu.weights))

        let bp64SetsRolloutCopy = (BP64SetsRollout.copy bp64SetsRollout)
        let mutable sortableIndex = 0
        while (sortableIndex < bp64SetsRollout.baseArray.Length) do
                switchRangeMakeSwitchUses 
                    sorter 
                    firstSwitchDex 
                    lastSwitchDex
                    switchUseB64
                    bp64SetsRolloutCopy
                    sortableIndex

                sortableIndex <- sortableIndex + (Degree.value sorter.degree)

        let switchUses = SwitchUseB64.toSwitchUses switchUseB64
        switchEventRecords.BySwitch {
            groupBySwitch.switchUses = switchUses; 
            groupBySwitch.sortableRollout = sortableSetRollout.Bp64
                                                bp64SetsRolloutCopy
        }




    // uses a sorter.switchcount length array to store accumulated
    // switch uses
    let private switchRangeMakeSwitchUsesSlice
                    (sorter:sorter) 
                    (mindex:int) (maxdex:int) 
                    (switchUseB64:SwitchUseB64) 
                    (sortableArray:uint64[]) =
        let useWeights = (SwitchUseB64.getWeights switchUseB64)
        let mutable looP = true
        let mutable localSwitchOffset = mindex
        while ((localSwitchOffset < maxdex) && looP) do
            let switch = sorter.switches.[localSwitchOffset]
            let lv = sortableArray.[switch.low]
            let hv = sortableArray.[switch.hi]
            let rv = useWeights.[localSwitchOffset]
            sortableArray.[switch.hi] <- (lv ||| hv)
            sortableArray.[switch.low] <- (lv &&& hv)
            useWeights.[localSwitchOffset] <- (((~~~hv) &&& lv) ||| rv)
            localSwitchOffset <- localSwitchOffset+1


    // creates a sorter.switchcount length array to store accumulated
    // switch uses
    let sorterMakeSwitchUsesSlice
                    (sorter:sorter) 
                    (bp64SetsRollout:bP64SetsRollout)  
                    (switchusePlan:Sorting.switchUsePlan) =

        let switchCount = (SwitchCount.value sorter.switchCount)
        let firstSwitchDex, lastSwitchDex, switchUseB64 =
            match switchusePlan with
            | Sorting.switchUsePlan.All -> 
                (0, switchCount, (SwitchUseB64.createEmpty sorter.switchCount))
            | Sorting.switchUsePlan.Range (min, max) -> 
                (min, max, (SwitchUseB64.createEmpty sorter.switchCount))
            | Sorting.switchUsePlan.Indexes (min, max, swu) -> 
                (min, max, (SwitchUseB64.init sorter.switchCount swu.weights))

        let bp64SetsRolloutCopy = (BP64SetsRollout.copy bp64SetsRollout)
        let sortableArray = Array.zeroCreate<uint64> (Degree.value sorter.degree)
        let mutable sortableIndex = 0
        while (sortableIndex < bp64SetsRollout.baseArray.Length) do
                ByteUtils.mapUint64Arrays sortableIndex bp64SetsRollout.baseArray 0 sortableArray (Degree.value sorter.degree)
                switchRangeMakeSwitchUsesSlice 
                    sorter 
                    firstSwitchDex 
                    lastSwitchDex
                    switchUseB64
                    sortableArray

                sortableIndex <- sortableIndex + (Degree.value sorter.degree)

        let switchUses = SwitchUseB64.toSwitchUses switchUseB64
        switchEventRecords.BySwitch {
            groupBySwitch.switchUses = switchUses; 
            groupBySwitch.sortableRollout = sortableSetRollout.Bp64
                                                bp64SetsRolloutCopy
        }




    let evalSorterOnBP64SetsRollout
                    (sorter:sorter)
                    (bp64SetsRollout:bP64SetsRollout)
                    (switchusePlan:Sorting.switchUsePlan) 
                    (switchEventAgg:Sorting.eventGrouping) =

        match switchEventAgg with
        | Sorting.eventGrouping.NoGrouping -> 
                sorterWithNoSAG
                      sorter 
                      bp64SetsRollout 
                      switchusePlan

        | Sorting.eventGrouping.BySwitch -> 
                sorterMakeSwitchUses 
                    sorter 
                    bp64SetsRollout 
                    switchusePlan


    let evalSorter (sorter:sorter)
                   (bpSr:bP64SetsRollout)
                   (switchusePlan:Sorting.switchUsePlan) 
                   (switchEventAgg:Sorting.eventGrouping) =
        evalSorterOnBP64SetsRollout
            sorter bpSr switchusePlan switchEventAgg



    module SorterSet =

        let eval<'T> 
                 (sorterSet:sorterSet)
                 (bP64SetsRollout:bP64SetsRollout)
                 (switchusePlan:Sorting.switchUsePlan) 
                 (switchEventAgg:Sorting.eventGrouping) 
                 (_parallel:UseParallel) 
                 (reporter:sortingResult -> Result<'T, string>) =

            let rewrap tup ssr proc = 
                let sorterId, sorter = tup
                let swEvRecs = evalSorterOnBP64SetsRollout 
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
                                                 |> Array.Parallel.map(fun s-> rewrap s bP64SetsRollout reporter)
                                                 |> Array.toList
                                                 |> Result.sequence
                    | false -> sorterSet.sorters |> Map.toList 
                                                 |> List.map(fun s-> rewrap s bP64SetsRollout reporter)
                                                 |> Result.sequence
            }


    module History =

        let sortTHistSwitches(switches:Switch list)
                             (pBits:bitsP64) =
            let mutable i = 0
            let mutable lstRet = [pBits]
            let mutable newCase = pBits

            while (i < switches.Length) do
                newCase <- newCase |> BitsP64.copy
                let uintArray = newCase.values
                let sw = switches.[i]
                let lv = uintArray.[sw.low]
                let hv = uintArray.[sw.hi]
                uintArray.[sw.hi] <- (lv ||| hv)
                uintArray.[sw.low] <- (lv &&& hv)
                lstRet <- newCase::lstRet
                i <- i + 1
            lstRet |> List.rev


        let sortTHistSwitchList (sorter:sorter) 
                                 (mindex:int) 
                                 (maxdex:int) 
                                 (pBits:bitsP64) =
            let sws = sorter.switches |> Array.skip(mindex)
                                      |> Array.take(maxdex - mindex)
                                      |> Array.toList
            sortTHistSwitches sws pBits


        let sortTHist (sorter:sorter) 
                      (pBits:bitsP64) =
            let sl = SwitchCount.value sorter.switchCount
            sortTHistSwitchList sorter 0 sl pBits

