﻿namespace global
open System

module Sorting =

    type switchUsePlan =
        | All 
        | Range of int*int
        | Indexes of int*int*switchUses

    module SwitchUsePlan = 

        let makeIndexes (prefixUses:switchUses) 
                        (totalCount:SwitchCount) =

            let prefixCount = prefixUses.weights.Length
            let scArray = 
                Array.init 
                    (SwitchCount.value totalCount)
                    (fun dex -> 
                            if (dex < prefixCount) 
                                    then prefixUses.weights.[dex] else 0)               

            (  prefixCount, 
               SwitchCount.value totalCount, 
               {switchUses.weights = scArray}
            )
            |> switchUsePlan.Indexes


    type eventGrouping =
        | NoGrouping
        | BySwitch


