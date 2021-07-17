namespace global
open System

module Sorting =

    type SwitchUsePlan =
        | All 
        | Range of int*int
        | Indexes of int*int*int[]

    module SwitchUsePlan = 

        let makeIndexes (prefixCount:SwitchCount) 
                        (totalCount:SwitchCount) =

            let scArray = 
                Array.init 
                    (SwitchCount.value totalCount)
                    (fun dex -> 
                            if (dex < (SwitchCount.value prefixCount)) 
                                    then 1 else 0)               

            ((SwitchCount.value prefixCount), 
             (SwitchCount.value totalCount), 
              scArray)
            |> SwitchUsePlan.Indexes


    type EventGrouping =
        | NoGrouping
        | BySwitch
