namespace global
open System
open SortingEval

module SortingOps =

    module Sorter =
        //let eval
        //        (sorter:sorter)
        //        (sortableSet:sortableSetO)
        //        (switchusePlan:Sorting.switchUsePlan) 
        //        (switchEventAgg:Sorting.eventGrouping) =

        //    match sortableSet with
        //    | Binary ssb -> 
        //           SortingInts.evalSorterOnBinary
        //                           sorter
        //                           ssb.sortables
        //                           switchusePlan
        //                           switchEventAgg
        //    | Integer ssb -> 
        //           SortingInts.evalSorterOnInteger 
        //                           sorter
        //                           ssb.sortables
        //                           switchusePlan
        //                           switchEventAgg
        //    | Bp64 ssb -> 
        //           SortingBp64.evalSorter
        //                          sorter
        //                          ssb.sortables
        //                          switchusePlan
        //                          switchEventAgg

        let eval2
                (sorter:sorter)
                (sortableSetImpl:sortableSetImpl)
                (switchusePlan:Sorting.switchUsePlan) 
                (switchEventAgg:Sorting.eventGrouping) =

            match sortableSetImpl with
            | sortableSetImpl.Binary (ssb, d) -> 
                   SortingInts.evalSorterOnBinary
                                   sorter
                                   ssb
                                   switchusePlan
                                   switchEventAgg
            | sortableSetImpl.Integer (ssb, d) -> 
                   SortingInts.evalSorterOnInteger 
                                   sorter
                                   ssb
                                   switchusePlan
                                   switchEventAgg
            | sortableSetImpl.Bp64 (ssb, d) -> 
                   SortingBp64.evalSorter
                                  sorter
                                  ssb
                                  switchusePlan
                                  switchEventAgg


        let evalR
                (sorter:sorter)
                (sortableSetRollout:sortableSetRollout)
                (switchusePlan:Sorting.switchUsePlan) 
                (switchEventAgg:Sorting.eventGrouping) =
                match sortableSetRollout with
                | sortableSetRollout.Int iSr ->
                    SortingInts.evalSorterOnIntSetsRollout 
                                        sorter
                                        iSr
                                        switchusePlan
                                        switchEventAgg
                | sortableSetRollout.Bp64 bpSr ->
                    SortingBp64.evalSorterOnBP64SetsRollout
                                        sorter
                                        bpSr
                                        switchusePlan
                                        switchEventAgg



    module SortableSet =

        //let switchReduce (sSet:sortableSetO) 
        //                 (switches:seq<Switch>) = 
        //    let degree = sSet |> SortableSet0.degree
        //    let sorter = Sorter.fromSwitches 
        //                    degree
        //                    switches

        //    let res = Sorter.eval
        //                    sorter
        //                    sSet
        //                    Sorting.switchUsePlan.All
        //                    Sorting.eventGrouping.BySwitch

        //    let switchUses = res |> SwitchEventRecords.getSwitchUses
        //    result {
        //        let! ssRoll = res |> SwitchEventRecords.getSortableSetRollout
        //                          |> SortableSetRollout.removeDupes
        //        let ssReduced = 
        //            match ssRoll with
        //            | sortableSetRollout.Bp64 bp64Roll -> 
        //               bp64Roll |> BP64SetsRollout.toBitSet
        //                        |> Seq.toArray
        //                        |> SortableSetBinary.fromBitSet degree
        //                        |> sortableSetO.Binary
        //            | sortableSetRollout.Int intRoll -> 
        //               intRoll |> IntSetsRollout.toIntSet
        //                       |> Seq.toArray
        //                       |> SortableSetInteger.fromIntSet degree
        //                       |> sortableSetO.Integer
        //        return (ssReduced, switchUses)
        //    }


        let switchReduce2 (ssImpl:sortableSetImpl) 
                          (pfxs:Switch[]) = 
            let ssid = ([(ssImpl :> obj); (pfxs :> obj);]) 
                        |> GuidUtils.guidFromObjList
                        |> SortableSetId.fromGuid
            let degree = ssImpl |> SortableSetImpl.getDegree
            let sorter = Sorter.fromSwitches 
                            degree
                            pfxs

            let res = Sorter.eval2
                            sorter
                            ssImpl
                            Sorting.switchUsePlan.All
                            Sorting.eventGrouping.BySwitch

            let switchUses = res |> SwitchEventRecords.getSwitchUses
            result {
                let! ssRoll = res |> SwitchEventRecords.getSortableSetRollout
                                  |> SortableSetRollout.removeDupes
                let ssReduced = 
                    match ssRoll with
                    | sortableSetRollout.Bp64 bp64Roll -> 
                        let bitSets = bp64Roll |> BP64SetsRollout.toBitsP64
                                               |> Seq.toArray
                        (bitSets, degree) |> sortableSetImpl.Bp64

                    | sortableSetRollout.Int intRoll -> 
                        let intSets = intRoll |> IntSetsRollout.toIntSet
                                              |> Seq.toArray
                        (intSets, degree) |> sortableSetImpl.Integer
   
                return (ssReduced, switchUses)
            }


        //let reduceByPrefix (srg:sorterRndGen) 
        //                   (sSet:sortableSetO) = 
        //    let pfx = srg |> SorterRndGen.getSwitchPrefix
        //    if pfx.Length = 0 then
        //        (sSet, SwitchUses.createNone) |> Ok
        //    else switchReduce sSet pfx
         

        let reduceByPrefix2 (srg:sorterRndGen) 
                            (ssImpl:sortableSetImpl)  =
            let pfx = srg |> SorterRndGen.getSwitchPrefix
                          |> List.toArray
            if pfx.Length = 0 then
                (ssImpl, SwitchUses.createNone) |> Ok
            else switchReduce2 ssImpl pfx

                
        //let oneStageReduceBp64 (degree:Degree) = 
            
        //    let wholeSet = SortableSetBp64.allBp64 degree
        //                   |> sortableSetO.Bp64

        //    let switches = degree |> TwoCycleGen.evenMode
        //                          |> Switch.fromTwoCyclePerm
        //    switchReduce wholeSet switches


        //let oneStageReduceInts (degree:Degree) = 
    
        //    let wholeSet = SortableSetBinary.allIntBits degree
        //                   |> sortableSetO.Binary

        //    let switches = degree |> TwoCycleGen.evenMode
        //                          |> Switch.fromTwoCyclePerm
        //    switchReduce wholeSet switches


    module SorterSet =

        //let eval<'T> 
        //        (sorterSet:sorterSet)
        //        (sortableSet:sortableSetO)
        //        (switchusePlan:Sorting.switchUsePlan) 
        //        (switchEventAgg:Sorting.eventGrouping) 
        //        (_parallel:UseParallel) 
        //        (proc:sortingResult -> Result<'T, string>) =
           
        //        match sortableSet with
        //        | Binary ssb -> 
        //                result {
        //                    let! intSetsRollout = ssb.sortables |> IntSetsRollout.fromBitSet
        //                                                            sorterSet.degree
        //                    return! SortingInts.SorterSet.eval 
        //                                sorterSet
        //                                intSetsRollout
        //                                ssb.id
        //                                switchusePlan
        //                                switchEventAgg
        //                                _parallel
        //                                proc
        //                }

        //        | Integer ssb -> 
        //                result {
        //                    let! intSetsRollout = 
        //                        ssb.sortables
        //                            |> Array.map(fun a -> a.values)
        //                            |> IntSetsRollout.fromIntArrays
        //                                                            sorterSet.degree
        //                    return! SortingInts.SorterSet.eval 
        //                                sorterSet
        //                                intSetsRollout
        //                                ssb.id
        //                                switchusePlan
        //                                switchEventAgg
        //                                _parallel
        //                                proc
        //                }

        //        | Bp64 ssb -> 
        //                    result {
        //                        let! bp64SetsRollout = ssb.sortables |> BP64SetsRollout.fromBitsP64
        //                                                            sorterSet.degree
        //                        return! SortingBp64.SorterSet.eval 
        //                                    sorterSet
        //                                    bp64SetsRollout
        //                                    ssb.id
        //                                    switchusePlan
        //                                    switchEventAgg
        //                                    _parallel
        //                                    proc
        //                    }


        let eval2<'T> 
                (sorterSet:sorterSet)
                (srtblSt:sortableSet)
                (switchusePlan:Sorting.switchUsePlan) 
                (switchEventAgg:Sorting.eventGrouping) 
                (_parallel:UseParallel) 
                (proc:sortingResult -> Result<'T, string>) =

            match srtblSt.sortableSetImpl with
            | sortableSetImpl.Binary (btSet, d) -> 
                result {
                    let! intSetsRollout = btSet |> IntSetsRollout.fromBitSet
                                                            sorterSet.degree
                    return! SortingInts.SorterSet.eval 
                                sorterSet
                                intSetsRollout
                                srtblSt.id
                                switchusePlan
                                switchEventAgg
                                _parallel
                                proc
                }
            | sortableSetImpl.Bp64 (bs64, d)  -> 
                result {
                    let! bp64Rollout = bs64 |> BP64SetsRollout.fromBitsP64
                                                          sorterSet.degree
                    return! SortingBp64.SorterSet.eval 
                                sorterSet
                                bp64Rollout
                                srtblSt.id
                                switchusePlan
                                switchEventAgg
                                _parallel
                                proc
                }
            | sortableSetImpl.Integer (ntS, d)  ->
                result {
                    let! intSetsRollout = ntS |> IntSetsRollout.fromIntSets
                                                           sorterSet.degree
                    return! SortingInts.SorterSet.eval 
                                sorterSet
                                intSetsRollout
                                srtblSt.id
                                switchusePlan
                                switchEventAgg
                                _parallel
                                proc
                    }


        //let getSorterCoverages 
        //    (sorterSet:sorterSet)
        //    (sortableSet:sortableSetO)
        //    (switchusePlan:Sorting.switchUsePlan)
        //    (checkSuccess:bool)
        //    (_parallel:UseParallel) =

        //   result {
        //       let! sorterCovs = 
        //                eval
        //                   sorterSet 
        //                   sortableSet 
        //                   switchusePlan
        //                   Sorting.eventGrouping.BySwitch
        //                   _parallel
        //                   (SortingEval.SorterCoverage.fromSwitchEventRecords true)

        //       return sorterCovs
        //   }

        let getSorterCoverages2
            (sorterSet:sorterSet)
            (sortableSet:sortableSet)
            (switchusePlan:Sorting.switchUsePlan)
            (checkSuccess:bool)
            (_parallel:UseParallel) =

           result {
               let! sorterCovs = 
                        eval2
                           sorterSet 
                           sortableSet 
                           switchusePlan
                           Sorting.eventGrouping.BySwitch
                           _parallel
                           (SortingEval.SorterCoverage.fromSwitchEventRecords true)

               return sorterCovs
           }


