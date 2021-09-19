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
                        let bitSets = bp64Roll |> BP64SetsRollout.toBitsP64
                                               |> Seq.toArray
                        (bitSets, degree) |> sortableSetImpl.Bp64

                    | sortableSetRollout.Int intRoll -> 
                        let intSets = intRoll |> IntSetsRollout.toIntSet
                                              |> Seq.toArray
                        (intSets, degree) |> sortableSetImpl.Integer
   
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
            | sortableSetImpl.Binary (btSet, d) -> 
                result {
                    let! intSetsRollout = btSet |> IntSetsRollout.fromBitSet
                                                            sorterSet.degree
                    return! SortingInts.SorterSet.eval 
                                sorterSet
                                intSetsRollout
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


