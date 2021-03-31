namespace global
open System

module SortingEval =

    type NoGrouping  = 
        {
            switchEventRollout:SwitchEventRollout; 
            sortableSetRollout:SortableSetRollout;
        }

    type GroupBySwitch = 
        {
            switchUses:SwitchUses; 
            sortableSetRollout:SortableSetRollout;
        }

    type GroupBySortable = 
        {
            sortableUses:SortableUses; 
            sortableSetRollout:SortableSetRollout;
        }

    type SwitchEventRecords =
        | NoGrouping of NoGrouping
        | BySwitch of GroupBySwitch
        | BySortable of GroupBySortable


    module SwitchEventRecords =
        let getSwitchUses (switchEventRecords:SwitchEventRecords) =
            match switchEventRecords with
            | NoGrouping seNg -> seNg.switchEventRollout 
                                    |> SwitchEventRollout.toSwitchUses
                                    |> Ok
            | BySwitch seGs -> seGs.switchUses 
                                    |> Ok
            | BySortable _ -> "switchUses not in GroupBySortable" |> Error


        let getHistogramOfSortedSortables (switchEventRecords:SwitchEventRecords) =
            match switchEventRecords with
            | NoGrouping seNg -> seNg.sortableSetRollout 
                                    |> SortableSetRollout.histogramOfSortedSortables
                                    |> Ok
            | BySwitch seGs ->  seGs.sortableSetRollout 
                                    |> SortableSetRollout.histogramOfSortedSortables
                                    |> Ok
            | BySortable seGt -> seGt.sortableSetRollout
                                      |> SortableSetRollout.histogramOfSortedSortables
                                      |> Ok

        let getAllSortsWereComplete (switchEventRecords:SwitchEventRecords) =
            match switchEventRecords with
            | NoGrouping seNg -> seNg.sortableSetRollout 
                                    |> SortableSetRollout.isSorted
                                    |> Ok
            | BySwitch seGs ->  seGs.sortableSetRollout
                                    |> SortableSetRollout.isSorted
                                    |> Ok
            | BySortable seGt -> seGt.sortableSetRollout
                                      |> SortableSetRollout.isSorted
                                      |> Ok

        let getUsedSwitchCount (switchEventRecords:SwitchEventRecords) =
            result {
                let! switchUses = getSwitchUses switchEventRecords
                return! switchUses |> SwitchUses.toUsedSwitchCount
            }

    type ResultOfSorterOnSortableSet =
        {
            sorter:Sorter; 
            switchEventRecords:SwitchEventRecords;
            sorterId:SorterId;
            sortableSetId:SortableSetId
        }

    type SorterCoverage = 
        { 
            usedSwitchCount:SwitchCount; 
            usedStageCount:StageCount;
            sorterId:SorterId;
            sortableSetId:SortableSetId
        }
                     
    module SorterCoverage = 
        let fromSwitchEventRecords 
                (r:ResultOfSorterOnSortableSet) =
            result {
                    let! switchUses = 
                            r.switchEventRecords |> SwitchEventRecords.getSwitchUses
                    let! usedSwitchArray = 
                            r.sorter |> SwitchUses.getUsedSwitches switchUses
                    let! usedSwitchCount = SwitchCount.create "" usedSwitchArray.Length
                    let! usedStageCount = Stage.getStageCount r.sorter.degree usedSwitchArray
                    return {
                                SorterCoverage.usedSwitchCount = usedSwitchCount; 
                                usedStageCount = usedStageCount;
                                sorterId = r.sorterId;
                                sortableSetId = r.sortableSetId
                           }
               }
        
    type SorterEff = 
        { 
            usedSwitchCount:SwitchCount; 
            usedStageCount:StageCount;
            sorterId:SorterId;
            sortableSetId:SortableSetId;
            sucessfulSort:bool
        }


    module SorterEff = 
        let fromSwitchEventRecords 
                (r:ResultOfSorterOnSortableSet) =
            result {
                    let! switchUses = 
                            r.switchEventRecords |> SwitchEventRecords.getSwitchUses
                    let! success = 
                            r.switchEventRecords |> SwitchEventRecords.getAllSortsWereComplete
                    let! usedSwitchArray = 
                            r.sorter |> SwitchUses.getUsedSwitches switchUses
                    let! usedSwitchCount = SwitchCount.create "" usedSwitchArray.Length
                    let! usedStageCount = Stage.getStageCount r.sorter.degree usedSwitchArray
                    return {
                                SorterEff.usedSwitchCount = usedSwitchCount; 
                                usedStageCount = usedStageCount;
                                sorterId = r.sorterId;
                                sortableSetId = r.sortableSetId;
                                sucessfulSort = success
                           }
               }

    type SorterPerfBin = 
        { 
            usedSwitchCount:SwitchCount; 
            usedStageCount:StageCount;
        }

    module SorterPerfBin = 
        let fromSorterEffs (sorterEffs:SorterEff list) = 
            sorterEffs 
            |> Seq.filter(fun eff->eff.sucessfulSort)
            |> Seq.map(fun eff ->
                         {SorterPerfBin.usedStageCount=eff.usedStageCount
                          SorterPerfBin.usedSwitchCount=eff.usedSwitchCount})
            |> Seq.countBy id
            |> Seq.toArray

        let repStr (sorterPerfBin:SorterPerfBin) =
            sprintf "%s\t%s"
                ((SwitchCount.value sorterPerfBin.usedSwitchCount) |> string)
                ((StageCount.value sorterPerfBin.usedStageCount) |> string)

        let binReport (bins:(SorterPerfBin*int)[]) = 
            let repLine (sorterPerfBin:SorterPerfBin) (ct:int) = 
                    sprintf "%s\t%s"
                        (repStr sorterPerfBin)
                        (ct|> string)
            bins |> StringUtils.printLinesOfArrayf
                    (fun tup -> repLine (fst tup) (snd tup))


    type SortingRecords = 
            | SorterCoverage of SorterCoverage
            | SorterEff of SorterEff
            | SorterPerfBins of (SorterPerfBin*int)[]


    module SortingRecords = 
        let getSorterCoverage (r:ResultOfSorterOnSortableSet) =
            result {
                let! sorterCoverage = r |> SorterCoverage.fromSwitchEventRecords
                return sorterCoverage
            }

        let getSorterEff (r:ResultOfSorterOnSortableSet) =
            result {
                let! sorterEff = r |> SorterEff.fromSwitchEventRecords
                return sorterEff 
        }
