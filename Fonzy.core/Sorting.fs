namespace global
open System

module Sorting =

    type SwitchUsePlan =
        | All 
        | Range of int*int
        | Indexes of int*int*int[]


    type EventGrouping =
        | NoGrouping
        | BySwitch
