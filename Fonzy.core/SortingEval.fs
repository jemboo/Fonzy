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
            | BySortable seGt -> "switchUses not in GroupBySortable" |> Error

        let getHistogramOfSortedSortables (switchEventRecords:SwitchEventRecords) =
            match switchEventRecords with
            | NoGrouping seNg -> seNg.sortableSetRollout 
                                    |> SortableSetRollout.histogramOfSortedSortables
                                    |> Ok
            | BySwitch seGs ->  "switchUses not in GroupBySortable" |> Error
            | BySortable seGt -> seGt.sortableSetRollout
                                      |> SortableSetRollout.histogramOfSortedSortables
                                      |> Ok

        let getUsedSwitchCount (switchEventRecords:SwitchEventRecords) =
            result {
                let! switchUses = getSwitchUses switchEventRecords
                return! switchUses |> SwitchUses.toUsedSwitchCount
            }

    type SorterPerf = { 
                        usedSwitchCount:SwitchCount; 
                        usedStageCount:StageCount;
                        sorterId:SorterId;
                        sortableSetId:SortableSetId
                     }

    module SorterPerf = 
        let fromSwitchEventRecords 
                (sorter:Sorter)
                (switchEventGrouping:SwitchEventRecords)
                (sorterId:SorterId)
                (sortableSetId:SortableSetId) =
            result {
                    let! switchUses = 
                            switchEventGrouping |> SwitchEventRecords.getSwitchUses
                    let! usedSwitchArray = 
                            sorter |> SwitchUses.getUsedSwitches switchUses
                    let! usedSwitchCount = SwitchCount.create "" usedSwitchArray.Length
                    let! usedStageCount = Stage.getStageCount sorter.degree usedSwitchArray
                    return {
                                SorterPerf.usedSwitchCount = usedSwitchCount; 
                                usedStageCount = usedStageCount;
                                sorterId = sorterId;
                                sortableSetId = sortableSetId
                           }
               }


    type SortingRecords = 
            | SorterPerf of SorterPerf



//type SortingResultsW = 
//    {
//        switchUses:SwitchUses;
//        successfulSortCount:SortableCount;
//        usedSwitchCount:SwitchCount;
//        usedStageCount:StageCount;
//        sortableSetId:Guid
//        sorterId:Guid
//    }

//module SortingResultsW = 
//    let headers =
//        [|"successfulSortCount"; "usedSwitchCount"; "usedStageCount"|]


//    let makeStandard (s:Sorter) (sorterId:Guid) (sortableSetId:Guid) 
//                     (su:SwitchUses) (sc:SortableCount) =
//        let w, t = (SwitchUses.getSwitchAndStageUses s su)
//        { 
//            SortingResultsW.switchUses = su;
//            successfulSortCount = sc;
//            usedSwitchCount = w;
//            usedStageCount = t;
//            sortableSetId = sortableSetId;
//            sorterId = sorterId;
//        }

//    let report (sstr:SortingResultsW) =
//        [|sprintf "%d" (SortableCount.value sstr.successfulSortCount);
//          sprintf "%d" (SwitchCount.value sstr.usedSwitchCount);
//          sprintf "%d" (StageCount.value sstr.usedStageCount);|]

//    let reportOpt (sstr:SortingResultsW option) =
//        match sstr with
//        | Some r -> report r
//        | None -> [|"";"";""|]