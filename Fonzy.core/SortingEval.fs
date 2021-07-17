namespace global


module SortingEval =

    type noGrouping  = 
        {
            switchEventRollout:switchEventRollout; 
            sortableRollout:sortableSetRollout;
        }

    type groupBySwitch = 
        {
            switchUses:SwitchUses; 
            sortableRollout:sortableSetRollout;
        }

    type switchEventRecords =
        | NoGrouping of noGrouping
        | BySwitch of groupBySwitch


    type sorterPerf = 
        { 
            usedSwitchCount:SwitchCount; 
            usedStageCount:StageCount;
            successful:bool Option
        }
                

    type sorterPerfBin = 
        { 
            usedSwitchCount:SwitchCount; 
            usedStageCount:StageCount;
            sorterCount:SorterCount;
            successCount:int;
            failCount:int;
        }


    module SwitchEventRecords =

        let getSortableSetRollout (switchEventRecords:switchEventRecords) =
            match switchEventRecords with
            | NoGrouping seNg -> seNg.sortableRollout
            | BySwitch seGs -> seGs.sortableRollout


        let getSwitchUses (switchEventRecords:switchEventRecords) =
            match switchEventRecords with
            | NoGrouping seNg -> seNg.switchEventRollout 
                                    |> SwitchEventRollout.toSwitchUses
            | BySwitch seGs -> seGs.switchUses


        let getHistogramOfSortedSortables (switchEventRecords:switchEventRecords) =
            match switchEventRecords with
            | NoGrouping seNg -> seNg.sortableRollout 
                                    |> SortableSetRollout.intBitsHist
            | BySwitch seGs ->  seGs.sortableRollout 
                                    |> SortableSetRollout.intBitsHist


        let getAllSortsWereComplete (switchEventRecords:switchEventRecords) =
            match switchEventRecords with
            | NoGrouping seNg -> seNg.sortableRollout 
                                    |> SortableSetRollout.isSorted
            | BySwitch seGs ->  seGs.sortableRollout
                                    |> SortableSetRollout.isSorted

        let getUsedSwitchCount (switchEventRecords:switchEventRecords) =
            result {
                let switchUses = getSwitchUses switchEventRecords
                return switchUses |> SwitchUses.usedSwitchCount
            }


    type sortingResult =
        {
            sorterId:SorterId;
            sortableSetId:SortableSetId
            sorter:Sorter; 
            switchEventRecords:switchEventRecords;
        }

    type sorterCoverage = 
        { 
            sorterId:SorterId;
            sortableSetId:SortableSetId;
            perf:sorterPerf; 
        }
        
                     
    module SorterCoverage = 

        let fromSwitchEventRecords (checkSuccess:bool)
                                   (r:sortingResult) =
            result {
                    let switchUses = 
                            r.switchEventRecords |> SwitchEventRecords.getSwitchUses
                    let usedSwitchArray = 
                            r.sorter |> SwitchUses.getUsedSwitches switchUses
                    let! usedSwitchCount = SwitchCount.create "" usedSwitchArray.Length
                    let usedStageCount = Stage.getStageCount r.sorter.degree usedSwitchArray
                    let success = 
                        match checkSuccess with
                        | true -> r.switchEventRecords 
                                  |> SwitchEventRecords.getAllSortsWereComplete
                                  |> Some
                        | false -> None

                    let perfBin = {
                                    sorterPerf.usedStageCount = usedStageCount;
                                    successful = success;
                                    usedSwitchCount=usedSwitchCount 
                                   }
                    return {
                            sorterCoverage.perf = perfBin; 
                            sorterId = r.sorterId;
                            sortableSetId = r.sortableSetId
                           }
               }


    module SorterPerfBin = 
    
        let fromSorterCoverage (coverage:sorterCoverage seq) =

            let extractSorterPerfBin ((stc, swc), (scs:sorterCoverage[])) =
                let sct = scs |> Array.filter(fun sc -> sc.perf.successful = (Some true))
                              |> Array.length
                let fct = scs |> Array.filter(fun sc -> sc.perf.successful = (Some false))
                              |> Array.length
                {
                    sorterPerfBin.sorterCount = SorterCount.fromInt scs.Length
                    usedStageCount = stc;
                    usedSwitchCount = swc;
                    successCount = sct;
                    failCount = fct;
                }

            coverage
                |> Seq.toArray
                |> Array.groupBy(fun c-> (c.perf.usedStageCount, 
                                          c.perf.usedSwitchCount))
                |> Array.map(extractSorterPerfBin)


    module SortingRecords = 
        let getSorterCoverage (checkSuccess:bool) 
                              (r:sortingResult) =
            result {
                let! sorterCoverage = r |> SorterCoverage.fromSwitchEventRecords 
                                                checkSuccess
                return sorterCoverage
            }
