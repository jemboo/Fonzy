namespace global
open System

module Sorting =

    type SwitchUsePlan =
        | All 
        | Range of int*int
        | Indexes of int*int*int[]

    module SwitchUsePlan = 

        let OneInit (degree:Degree) 
                    (stageCount:StageCount)  
                    (switchCount:SwitchCount)=
            let usedSwitchCt = (Degree.value degree) *
                               (StageCount.value stageCount)
            let scArray = 
                Array.init 
                    (SwitchCount.value switchCount)
                    (fun dex -> if (dex < usedSwitchCt) then 1 else 0)               

            (usedSwitchCt, (SwitchCount.value switchCount), scArray)
                |> SwitchUsePlan.Indexes



    type EventGrouping =
        | NoGrouping
        | BySwitch
