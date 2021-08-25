namespace global
open System
open SortingEval

module SortingOps =

    module Sorter =
        let eval
                (sorter:sorter)
                (sortableSet:sortableSet)
                (switchusePlan:Sorting.switchUsePlan) 
                (switchEventAgg:Sorting.eventGrouping) =

            match sortableSet with
            | Binary ssb -> 
                   SortingInts.evalSorterOnBinary
                                   sorter
                                   ssb
                                   switchusePlan
                                   switchEventAgg
            | Integer ssb -> 
                   SortingInts.evalSorterOnInteger 
                                   sorter
                                   ssb
                                   switchusePlan
                                   switchEventAgg
            | Bp64 ssb -> 
                      SortingBp64.evalSorter
                                  sorter
                                  ssb
                                  switchusePlan
                                  switchEventAgg


    module SortableSet =

        let switchReduce (sSet:sortableSet) 
                         (switches:seq<Switch>) = 
            let degree = sSet |> SortableSet.degree
            let sorter = Sorter.fromSwitches 
                            degree
                            switches

            let res = Sorter.eval
                            sorter
                            sSet
                            Sorting.switchUsePlan.All
                            Sorting.eventGrouping.BySwitch

            let switchUses = res |> SwitchEventRecords.getSwitchUses

            let uniBts = res |> SwitchEventRecords.getSortableSetRollout
                             |> SortableSetRollout.removeDupes
                             |> Seq.toArray

            let reducedSSet = 
                match sSet with
                | sortableSet.Binary _ -> 
                    uniBts
                        |> SortableSetBinary.fromIntBits degree
                        |> sortableSet.Binary
                | sortableSet.Bp64 _ -> 
                    uniBts
                        |> SortableSetBp64.fromIntBits degree
                        |> sortableSet.Bp64
                | sortableSet.Integer _ -> 
                    uniBts
                        |> SortableSetInteger.fromIntBits degree
                        |> sortableSet.Integer

            (reducedSSet, switchUses)

        let reduceByPrefix (srg:sorterRndGen) 
                           (sSet:sortableSet) = 
            let pfx = srg |> SorterRndGen.getSwitchPrefix
            if pfx.Length = 0 then
                (sSet, SwitchUses.createNone)
            else switchReduce sSet pfx
                

        let oneStageReduceBp64 (degree:Degree) = 
            
            let wholeSet = SortableSetBp64.allBp64 degree
                           |> sortableSet.Bp64

            let switches = degree |> TwoCycleGen.evenMode
                                  |> Switch.fromTwoCyclePerm
            switchReduce wholeSet switches


        let oneStageReduceInts (degree:Degree) = 
    
            let wholeSet = SortableSetBinary.allIntBits degree
                           |> sortableSet.Binary

            let switches = degree |> TwoCycleGen.evenMode
                                  |> Switch.fromTwoCyclePerm
            switchReduce wholeSet switches



    module SorterSet =

      let eval<'T> 
             (sorterSet:sorterSet)
             (sortableSet:sortableSet)
             (switchusePlan:Sorting.switchUsePlan) 
             (switchEventAgg:Sorting.eventGrouping) 
             (_parallel:UseParallel) 
             (proc:sortingResult -> Result<'T, string>) =
             
             match sortableSet with
             | Binary ssb -> 
                    result {
                        let! intSetsRollout = ssb.sortables |> IntSetsRollout.fromIntBits
                                                               sorterSet.degree
                        return! SortingInts.SorterSet.eval 
                                    sorterSet
                                    intSetsRollout
                                    ssb.id
                                    switchusePlan
                                    switchEventAgg
                                    _parallel
                                    proc
                    }

             | Integer ssb -> 
                    result {
                        let! intSetsRollout = ssb.sortables |> IntSetsRollout.fromIntArrays
                                                               sorterSet.degree
                        return! SortingInts.SorterSet.eval 
                                    sorterSet
                                    intSetsRollout
                                    ssb.id
                                    switchusePlan
                                    switchEventAgg
                                    _parallel
                                    proc
                    }

             | Bp64 ssb -> 
                       result {
                           let! bp64SetsRollout = ssb.sortables |> BP64SetsRollout.fromBitsP64
                                                                  sorterSet.degree
                           return! SortingBp64.SorterSet.eval 
                                       sorterSet
                                       bp64SetsRollout
                                       ssb.id
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
                            (SortingEval.SorterCoverage.fromSwitchEventRecords true)

                return sorterCovs
            }
