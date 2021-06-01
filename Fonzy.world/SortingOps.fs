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
             (proc:ResultOfSorterOnSortableSet -> Result<'T, string>) =

             match sortableSet with
             | Binary ssb -> 
                    result {
                        let! intSetsRollout = ssb.sortables |> IntSetsRollout.fromIntBits
                                                               sorterSet.degree
                        return! SortingInts.SorterSet.eval0 
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
                        return! SortingInts.SorterSet.eval0 
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
                           let! intSetsRollout = ssb.sortables |> BP64SetsRollout.fromBitsP64
                                                                  sorterSet.degree
                           return! SortingBp64.SorterSet.eval0 
                                       sorterSet
                                       intSetsRollout
                                       ssb.id
                                       switchusePlan
                                       switchEventAgg
                                       _parallel
                                       proc
                       }


      let getSorterPerfBins 
            (sorterSet:SorterSet)
            (sortableSet:SortableSet)
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
