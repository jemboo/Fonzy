namespace global
open System
open SortingEval

module SortingOps =

    module SorterSet =

      let eval<'T> 
             (sorterSet:SorterSet)
             (sortableSet:SortableSet)
             (switchusePlan:Sorting.SwitchUsePlan) 
             (switchEventAgg:Sorting.EventGrouping) 
             (_parallel:UseParallel) 
             (proc:SortingResult -> Result<'T, string>) =

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
                        let! bp64SetsRollout = ssb.sortables |> IntSetsRollout.fromIntArrays
                                                               sorterSet.degree
                        return! SortingInts.SorterSet.eval 
                                    sorterSet
                                    bp64SetsRollout
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


      let getSorterCoverageBins 
            (sorterSet:SorterSet)
            (sortableSet:SortableSet)
            (switchusePlan:Sorting.SwitchUsePlan)
            (checkSuccess:bool)
            (_parallel:UseParallel) =

            result {
                let! sorterEffs = 
                        eval
                            sorterSet 
                            sortableSet 
                            switchusePlan
                            Sorting.EventGrouping.BySwitch
                            _parallel
                            (SortingEval.SortingRecords.getSorterCoverage
                                                checkSuccess)

                let bins = sorterEffs 
                                |> SortingEval.SorterPerf.fromSorterEffs

                return bins
            }
