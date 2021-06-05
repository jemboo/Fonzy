namespace global
open System

module SortingEval =

    type NoGrouping  = 
        {
            switchEventRollout:switchEventRollout; 
            sortableRollout:SortableRollout;
        }

    type GroupBySwitch = 
        {
            switchUses:SwitchUses; 
            sortableRollout:SortableRollout;
        }

    type SwitchEventRecords =
        | NoGrouping of NoGrouping
        | BySwitch of GroupBySwitch

    type SorterPerf = 
        { 
            usedSwitchCount:SwitchCount; 
            usedStageCount:StageCount;
        }


    module SwitchEventRecords =

        let getSwitchUses (switchEventRecords:SwitchEventRecords) =
            match switchEventRecords with
            | NoGrouping seNg -> seNg.switchEventRollout 
                                    |> SwitchEventRollout.toSwitchUses
                                    |> Ok
            | BySwitch seGs -> seGs.switchUses 
                                    |> Ok


        let getHistogramOfSortedSortables (switchEventRecords:SwitchEventRecords) =
            match switchEventRecords with
            | NoGrouping seNg -> seNg.sortableRollout 
                                    |> SortableRollout.intBitsHist
                                    |> Ok
            | BySwitch seGs ->  seGs.sortableRollout 
                                    |> SortableRollout.intBitsHist
                                    |> Ok

        let getAllSortsWereComplete (switchEventRecords:SwitchEventRecords) =
            match switchEventRecords with
            | NoGrouping seNg -> seNg.sortableRollout 
                                    |> SortableRollout.isSorted
                                    |> Ok
            | BySwitch seGs ->  seGs.sortableRollout
                                    |> SortableRollout.isSorted
                                    |> Ok

        let getUsedSwitchCount (switchEventRecords:SwitchEventRecords) =
            result {
                let! switchUses = getSwitchUses switchEventRecords
                return switchUses |> SwitchUses.usedSwitchCount
            }

    type SortingResult =
        {
            sorterId:SorterId;
            sortableSetId:SortableSetId
            sorter:Sorter; 
            switchEventRecords:SwitchEventRecords;
        }

    type SorterCoverage = 
        { 
            sorterId:SorterId;
            sortableSetId:SortableSetId;
            sorterPerfBin:SorterPerf; 
        }
        
    type SorterEff = 
        { 
            sorterCoverage:SorterCoverage;
            sucessfulSort:bool
        }

                     
    module SorterCoverage = 

        let fromSwitchEventRecords (r:SortingResult) =
            result {
                    let! switchUses = 
                            r.switchEventRecords |> SwitchEventRecords.getSwitchUses
                    let! usedSwitchArray = 
                            r.sorter |> SwitchUses.getUsedSwitches switchUses
                    let! usedSwitchCount = SwitchCount.create "" usedSwitchArray.Length
                    let! usedStageCount = Stage.getStageCount r.sorter.degree usedSwitchArray
                    let perfBin = {SorterPerf.usedStageCount = usedStageCount;
                                   usedSwitchCount=usedSwitchCount }
                    return {
                                SorterCoverage.sorterPerfBin = perfBin; 
                                sorterId = r.sorterId;
                                sortableSetId = r.sortableSetId
                           }
               }
        

    module SorterEff = 

        let fromSwitchEventRecords (r:SortingResult) =
            result {
                    let! sorterCoverage = 
                            r |> SorterCoverage.fromSwitchEventRecords
                    let! success = 
                            r.switchEventRecords |> SwitchEventRecords.getAllSortsWereComplete
                    return {
                                SorterEff.sorterCoverage = sorterCoverage;
                                sucessfulSort = success
                           }
               }


    module SorterPerf = 

        let fromSorterEffs (sorterEffs:SorterEff list) = 
            sorterEffs 
            |> Seq.filter(fun eff->eff.sucessfulSort)
            |> Seq.map(fun eff -> eff.sorterCoverage)
            |> Seq.countBy id
            |> Seq.toArray

        let repStr (sorterPerfBin:SorterPerf) =
            sprintf "%s\t%s"
                ((SwitchCount.value sorterPerfBin.usedSwitchCount) |> string)
                ((StageCount.value sorterPerfBin.usedStageCount) |> string)

        let binReport (bins:(SorterPerf*int)[]) = 
            let repLine (sorterPerfBin:SorterPerf) (ct:int) = 
                    sprintf "%s\t%s"
                        (repStr sorterPerfBin)
                        (ct|> string)
            bins |> StringUtils.printLinesOfArrayf
                    (fun tup -> repLine (fst tup) (snd tup))


    type SortingRecords = 
            | SorterCoverage of SorterCoverage
            | SorterEff of SorterEff
            | SorterPerfBins of (SorterPerf*int)[]


    module SortingRecords = 
        let getSorterCoverage (r:SortingResult) =
            result {
                let! sorterCoverage = r |> SorterCoverage.fromSwitchEventRecords
                return sorterCoverage
            }

        let getSorterEff (r:SortingResult) =
            result {
                let! sorterEff = r |> SorterEff.fromSwitchEventRecords
                return sorterEff 
        }
