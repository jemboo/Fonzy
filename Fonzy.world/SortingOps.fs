namespace global
open System
open SortingEval

module SortingOps =

    module Sorter =
    
        let evalRollout
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
    
    
        let eval
                (sorter:sorter)
                (sortableSetImpl:sortableSetImpl)
                (switchusePlan:Sorting.switchUsePlan) 
                (switchEventAgg:Sorting.eventGrouping) =

            match sortableSetImpl with
            | sortableSetImpl.Binary isr -> 
                   SortingInts.evalSorterOnBinary
                                   sorter
                                   isr
                                   switchusePlan
                                   switchEventAgg
            | sortableSetImpl.Integer isr -> 
                   SortingInts.evalSorterOnInteger 
                                   sorter
                                   isr
                                   switchusePlan
                                   switchEventAgg
            | sortableSetImpl.Bp64 bpr -> 
                   SortingBp64.evalSorter
                                  sorter
                                  bpr
                                  switchusePlan
                                  switchEventAgg



    module SortableSet =

        let switchReduce (ssImpl:sortableSetImpl) 
                         (pfxs:Switch[]) = 

            let degree = ssImpl |> SortableSetImpl.getDegree
            let sorter = Sorter.fromSwitches 
                            degree
                            pfxs

            let res = Sorter.eval
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
                        bp64Roll |> sortableSetImpl.Bp64

                    | sortableSetRollout.Int intRoll -> 
                        intRoll |> sortableSetImpl.Integer

                //let ssReduced = 
                //    match ssRoll with
                //    | sortableSetRollout.Bp64 bp64Roll -> 
                //        let bitSets = bp64Roll |> BP64SetsRollout.toBitsP64
                //                               |> Seq.toArray
                //        (bitSets, degree) |> sortableSetImpl.Bp64

                //    | sortableSetRollout.Int intRoll -> 
                //        let intSets = intRoll |> IntSetsRollout.toIntSet
                //                              |> Seq.toArray
                //        (intSets, degree) |> sortableSetImpl.Integer
   
                return (ssReduced, switchUses)
            }


        let reduceBySorterRndGen (srg:sorterRndGen) 
                                 (ssImpl:sortableSetImpl)  =
            let pfx = srg |> SorterRndGen.getSwitchPrefix
                          |> List.toArray
            if pfx.Length = 0 then
                (ssImpl, SwitchUses.createNone) |> Ok
            else switchReduce ssImpl pfx


                
    module SorterSet =

      let eval<'T> 
            (sorterSet:sorterSet)
            (srtblSt:sortableSet)
            (switchusePlan:Sorting.switchUsePlan) 
            (switchEventAgg:Sorting.eventGrouping) 
            (_parallel:UseParallel) 
            (proc:sortingResult -> Result<'T, string>) =

            match srtblSt.sortableSetImpl with
            | sortableSetImpl.Binary isr -> 
                result {
                    //let! intSetsRollout = btSet |> IntSetsRollout.fromBitSet
                    //                                        sorterSet.degree
                    return! SortingInts.SorterSet.eval 
                                sorterSet
                                isr
                                switchusePlan
                                switchEventAgg
                                _parallel
                                proc
                }
            | sortableSetImpl.Bp64 bs64r  -> 
                result {
                    //let! bp64Rollout = bs64 |> BP64SetsRollout.fromBitsP64
                    //                                        sorterSet.degree
                    return! SortingBp64.SorterSet.eval 
                                sorterSet
                                bs64r
                                switchusePlan
                                switchEventAgg
                                _parallel
                                proc
                }
            | sortableSetImpl.Integer isr  ->
                result {
                    //let! intSetsRollout = ntS |> IntSetsRollout.fromIntSets
                    //                                        sorterSet.degree
                    return! SortingInts.SorterSet.eval 
                                sorterSet
                                isr
                                switchusePlan
                                switchEventAgg
                                _parallel
                                proc
                    }



      let getSorterCoverages
            (sorterSet:sorterSet)
            (sortableSet:sortableSet)
            (switchusePlan:Sorting.switchUsePlan)
            (checkSuccess:bool)
            (_parallel:UseParallel) =

            result {
                let! sorterCovs = 
                        eval
                            sorterSet 
                            sortableSet 
                            switchusePlan
                            Sorting.eventGrouping.BySwitch
                            _parallel
                            (SortingEval.SorterCoverage.fromSwitchEventRecords checkSuccess)

                return sorterCovs
            }


