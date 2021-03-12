﻿namespace global
open System

module SortingEval =

    type SwitchUsePlan =
        | All 
        | Range of int*int


    type ResultsNoSAG  = 
        {
            switchEventRollout:SwitchEventRollout; 
            sortableSetRollout:SortableSetRollout;
        }

    type ResultsSAGbySwitch = 
        {
            switchUses:SwitchUses; 
            sortableSetRollout:SortableSetRollout;
        }

    type ResultsSAGbySortable = 
        {
            sortableUses:SortableUses; 
            sortableSetRollout:SortableSetRollout;
        }

    type Results =
        | NoSAG of ResultsNoSAG
        | SAGbySwitch of ResultsSAGbySwitch
        | SAGbySortable of ResultsSAGbySortable

    type SwitchEventAgg =
        | NoAgg
        | BySwitch
        | BySortable


    type SorterEvalParams =
        {
            switchusePlan:SwitchUsePlan;
            sortableSet:SortableSet;
            switchEventAggregation:SwitchEventAgg;
            sorter:Sorter;
        }






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