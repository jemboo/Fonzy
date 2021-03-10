namespace global
open System


type SortingStrategy =
     | RunAllSwitches
     | QuitWhenSorted

type SwitchusePlan =
    | All 
    | Range of int*int

type SorterEvalParams =
    {
        sortingStrategy:SortingStrategy;
        switchusePlan:SwitchusePlan;
        sortableSet:SortableSet;
        sorterId:Guid;
        sorter:Sorter;
    }

type SorterEvalResultsBasic = 
    {
        switchCount:SwitchCount; 
        stagecount:StageCount;
        allAreSucessful:bool;
        sortableSetId:Guid;
        sorterId:Guid;
    }

type SorterEvalResultsHistogram = 
    {
        switchUses:SwitchUses;
        switchusePlan:SwitchusePlan;
        resultsHistogram: (SortableIntArray*int)[]
        sortableSetId:Guid;
        sorterId:Guid;
    }

type SorterEvalResultsRaw = 
    {
        switchUseRollout:SwitchUseRollout;
        switchusePlan:SwitchusePlan;
        sortableSetRollout: SortableSetRollout
        sortableSetId:Guid;
        sorterId:Guid;
    }

type SorterEvalResults =
    | Basic of SorterEvalResultsBasic
    | Histogram of SorterEvalResultsHistogram
    | Raw of SorterEvalResultsRaw

type SorterEvalReporting =
    | Basic
    | Histogram
    | Raw


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