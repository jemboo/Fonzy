namespace global
open System

//open Microsoft.FSharp.Collections

type SortingResults = 
    {
        switchUses:SwitchUses;
        successfulSortCount:SortableCount;
        usedSwitchCount:SwitchCount;
        usedStageCount:StageCount;
        sortableSetId:Guid
        sorterId:Guid
    }

module SortingResults = 
    let headers =
        [|"successfulSortCount"; "usedSwitchCount"; "usedStageCount"|]


    let makeStandard (s:Sorter) (sorterId:Guid) (sortableSetId:Guid) 
                     (su:SwitchUses) (sc:SortableCount) =
        let w, t = (SwitchUses.getSwitchAndStageUses s su)
        { 
            SortingResults.switchUses = su;
            successfulSortCount = sc;
            usedSwitchCount = w;
            usedStageCount = t;
            sortableSetId = sortableSetId;
            sorterId = sorterId;
        }

    let report (sstr:SortingResults) =
        [|sprintf "%d" (SortableCount.value sstr.successfulSortCount);
          sprintf "%d" (SwitchCount.value sstr.usedSwitchCount);
          sprintf "%d" (StageCount.value sstr.usedStageCount);|]

    let reportOpt (sstr:SortingResults option) =
        match sstr with
        | Some r -> report r
        | None -> [|"";"";""|]