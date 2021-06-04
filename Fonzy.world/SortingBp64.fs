namespace global
open System
open SortingEval

module SortingBp64 =

    let private switchRangeWithNoSAG 
                (sorter:Sorter) 
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
                (sorter:Sorter) 
                (bP64SetsRollout:bP64SetsRollout) 
                (switchusePlan:Sorting.SwitchUsePlan) =

        let switchCount = (SwitchCount.value sorter.switchCount)
        let firstSwitchDex, lastSwitchDex = 
            match switchusePlan with
            | Sorting.SwitchUsePlan.All -> (0, switchCount)
            | Sorting.SwitchUsePlan.Range (min, max) -> (min, max)
        
        let bPsRollCopy = BP64SetsRollout.copy bP64SetsRollout
        let seRollbp64 = SwitchEventRolloutBp64.create
                                sorter.switchCount
                                bPsRollCopy.sortableCount

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

        SwitchEventRecords.NoGrouping {
            NoGrouping.switchEventRollout = seRollbp64 |> switchEventRollout.Bp64
            NoGrouping.sortableRollout = bPsRollCopy |> SortableRollout.Bp64 
                                                
        }


    // uses a sorter.switchcount length array to store accumulated
    // switch uses
    let private switchRangeMakeSwitchUses 
                    (sorter:Sorter) 
                    (mindex:int) (maxdex:int) 
                    (switchUseB64:SwitchUseB64) 
                    (bp64SetsRollout:bP64SetsRollout) 
                    (sortableIndex:int) =
        let useWeights = (SwitchUseB64.getWeights switchUseB64)
        let sortableSetRolloutOffset = sortableIndex
        let mutable looP = true
        let mutable localSwitchOffset = mindex
        while ((localSwitchOffset < maxdex) && looP) do
            let switch = sorter.switches.[localSwitchOffset]
            let lv = bp64SetsRollout.baseArray.[switch.low + sortableSetRolloutOffset]
            let hv = bp64SetsRollout.baseArray.[switch.hi + sortableSetRolloutOffset]
            let rv = useWeights.[localSwitchOffset]
            bp64SetsRollout.baseArray.[switch.hi + sortableSetRolloutOffset] <- (lv ||| hv)
            bp64SetsRollout.baseArray.[switch.low + sortableSetRolloutOffset] <- (lv &&& hv)
            useWeights.[localSwitchOffset] <- (((~~~hv) &&& lv) ||| rv)
            localSwitchOffset <- localSwitchOffset+1


    // creates a sorter.switchcount length array to store accumulated
    // switch uses
    let sorterMakeSwitchUses
                    (sorter:Sorter) 
                    (bp64SetsRollout:bP64SetsRollout)  
                    (switchusePlan:Sorting.SwitchUsePlan) =

        let switchCount = (SwitchCount.value sorter.switchCount)
        let firstSwitchDex, lastSwitchDex =
            match switchusePlan with
            | Sorting.SwitchUsePlan.All -> (0, switchCount)
            | Sorting.SwitchUsePlan.Range (min, max) -> (min, max)
        let switchUseB64 = SwitchUseB64.createEmpty sorter.switchCount
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
                         |> Result.ExtractOrThrow
        SwitchEventRecords.BySwitch {
            GroupBySwitch.switchUses = switchUses; 
            GroupBySwitch.sortableRollout = SortableRollout.Bp64
                                                bp64SetsRolloutCopy
        }


    let evalSorterOnBP64SetsRollout
                    (sorter:Sorter)
                    (bp64SetsRollout:bP64SetsRollout)
                    (switchusePlan:Sorting.SwitchUsePlan) 
                    (switchEventAgg:Sorting.EventGrouping) =

        match switchEventAgg with
        | Sorting.EventGrouping.NoGrouping -> 
                sorterWithNoSAG
                      sorter 
                      bp64SetsRollout 
                      switchusePlan

        | Sorting.EventGrouping.BySwitch -> 
                sorterMakeSwitchUses 
                    sorter 
                    bp64SetsRollout 
                    switchusePlan


    let evalSorter (sorter:Sorter)
                   (sortableSet:SortableSetBp64)
                   (switchusePlan:Sorting.SwitchUsePlan) 
                   (switchEventAgg:Sorting.EventGrouping) =
        let sortableSetRollout = 
            sortableSet.sortables
                |> BP64SetsRollout.fromBitsP64
                        sorter.degree
                |> Result.ExtractOrThrow
        evalSorterOnBP64SetsRollout
            sorter sortableSetRollout switchusePlan switchEventAgg



    module SorterSet =

        let eval<'T> 
                 (sorterSet:SorterSet)
                 (bP64SetsRollout:bP64SetsRollout)
                 (sortableSetId:SortableSetId)
                 (switchusePlan:Sorting.SwitchUsePlan) 
                 (switchEventAgg:Sorting.EventGrouping) 
                 (_parallel:UseParallel) 
                 (proc:ResultOfSorterOnSortableSet -> Result<'T, string>) =

            let rewrap tup ssr = 
                let sorterId, sorter = tup
                let swEvRecs = evalSorterOnBP64SetsRollout 
                                    sorter ssr switchusePlan switchEventAgg
                let resSoSS = {
                    ResultOfSorterOnSortableSet.sorter = sorter;
                    ResultOfSorterOnSortableSet.switchEventRecords = swEvRecs;
                    ResultOfSorterOnSortableSet.sorterId = sorterId;
                    ResultOfSorterOnSortableSet.sortableSetId = sortableSetId
                }
                proc resSoSS

            result  {
                return!
                    match UseParallel.value(_parallel) with
                    | true  -> sorterSet.sorters |> Map.toArray 
                                                 |> Array.Parallel.map(fun s-> rewrap s bP64SetsRollout)
                                                 |> Array.toList
                                                 |> Result.sequence
                    | false -> sorterSet.sorters |> Map.toList 
                                                 |> List.map(fun s-> rewrap s bP64SetsRollout)
                                                 |> Result.sequence
            }


        let getSorterPerfBins 
            (sorterSet:SorterSet)
            (sortableSet:SortableSetBp64)
            (switchusePlan:Sorting.SwitchUsePlan)
            (_parallel:UseParallel) =
            result {

                let! ssRoll = sortableSet.sortables 
                              |> BP64SetsRollout.fromBitsP64
                                    sorterSet.degree

                let! sorterEffs =
                        eval 
                            sorterSet
                            ssRoll
                            sortableSet.id
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
                                 (pBits:bitsP64) =
            let sws = sorter.switches |> Array.skip(mindex)
                                      |> Array.take(maxdex - mindex)
                                      |> Array.toList
            sortTHistSwitches sws pBits


        let sortTHist (sorter:Sorter) 
                      (pBits:bitsP64) =
            let sl = SwitchCount.value sorter.switchCount
            sortTHistSwitchList sorter 0 sl pBits

