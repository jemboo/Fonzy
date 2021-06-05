namespace global

module SortingEval =

    type noGrouping  = 
        {
            switchEventRollout:switchEventRollout; 
            sortableRollout:SortableRollout;
        }

    type groupBySwitch = 
        {
            switchUses:SwitchUses; 
            sortableRollout:SortableRollout;
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

        let getSwitchUses (switchEventRecords:switchEventRecords) =
            match switchEventRecords with
            | NoGrouping seNg -> seNg.switchEventRollout 
                                    |> SwitchEventRollout.toSwitchUses
                                    |> Ok
            | BySwitch seGs -> seGs.switchUses 
                                    |> Ok


        let getHistogramOfSortedSortables (switchEventRecords:switchEventRecords) =
            match switchEventRecords with
            | NoGrouping seNg -> seNg.sortableRollout 
                                    |> SortableRollout.intBitsHist
                                    |> Ok
            | BySwitch seGs ->  seGs.sortableRollout 
                                    |> SortableRollout.intBitsHist
                                    |> Ok

        let getAllSortsWereComplete (switchEventRecords:switchEventRecords) =
            match switchEventRecords with
            | NoGrouping seNg -> seNg.sortableRollout 
                                    |> SortableRollout.isSorted
                                    |> Ok
            | BySwitch seGs ->  seGs.sortableRollout
                                    |> SortableRollout.isSorted
                                    |> Ok

        let getUsedSwitchCount (switchEventRecords:switchEventRecords) =
            result {
                let! switchUses = getSwitchUses switchEventRecords
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
            sorterPerf:sorterPerf; 
        }
        
                     
    module SorterCoverage = 

        let fromSwitchEventRecords (checkSuccess:bool) (r:sortingResult) =
            result {
                    let! switchUses = 
                            r.switchEventRecords |> SwitchEventRecords.getSwitchUses
                    let! usedSwitchArray = 
                            r.sorter |> SwitchUses.getUsedSwitches switchUses
                    let! usedSwitchCount = SwitchCount.create "" usedSwitchArray.Length
                    let! usedStageCount = Stage.getStageCount r.sorter.degree usedSwitchArray
                    let! success = 
                        match checkSuccess with
                        | true -> r.switchEventRecords 
                                  |> SwitchEventRecords.getAllSortsWereComplete
                                  |> Result.map Some
                        | false -> None |> Ok

                    let perfBin = {sorterPerf.usedStageCount = usedStageCount;
                                   successful = success;
                                   usedSwitchCount=usedSwitchCount }
                    return {
                            sorterCoverage.sorterPerf = perfBin; 
                            sorterId = r.sorterId;
                            sortableSetId = r.sortableSetId
                           }
               }


    module SorterPerfBin = 
    
        let fromSorterCoverage (coverage:sorterCoverage seq) =

            let extractSorterPerfBin ((stc, swc), (scs:sorterCoverage[])) =
                let succ = scs |> Array.filter(fun sc -> sc.sorterPerf.successful = (Some true))
                               |> Array.length
                let fayl = scs |> Array.filter(fun sc -> sc.sorterPerf.successful = (Some false))
                               |> Array.length
                {
                    sorterPerfBin.sorterCount = SorterCount.fromInt scs.Length
                    usedStageCount = stc;
                    usedSwitchCount = swc;
                    successCount = succ;
                    failCount = fayl;
                }

            let gp = coverage
                        |> Seq.toArray
                        |> Array.groupBy(fun c-> (c.sorterPerf.usedStageCount, 
                                                  c.sorterPerf.usedSwitchCount))
                        |> Array.map(extractSorterPerfBin)
            gp


    module SorterPerfR = 

        let repStr (sorterPerfBin:sorterPerf) =
            sprintf "%s\t%s"
                ((SwitchCount.value sorterPerfBin.usedSwitchCount) |> string)
                ((StageCount.value sorterPerfBin.usedStageCount) |> string)

        let binReport (bins:(sorterPerf*int)[]) = 
            let repLine (sorterPerfBin:sorterPerf) (ct:int) = 
                    sprintf "%s\t%s"
                        (repStr sorterPerfBin)
                        (ct|> string)
            bins |> StringUtils.printLinesOfArrayf
                    (fun tup -> repLine (fst tup) (snd tup))


    type sortingRecords = 
            | SorterCoverage of sorterCoverage
            | SorterPerfBins of sorterPerfBin[]


    module SortingRecords = 
        let getSorterCoverage (checkSuccess:bool) (r:sortingResult) =
            result {
                let! sorterCoverage = r |> SorterCoverage.fromSwitchEventRecords 
                                                checkSuccess
                return sorterCoverage
            }
