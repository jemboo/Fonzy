namespace global
open System

module Causes =
    let noOp =
        {Cause.causeSpec=CauseSpec.noOpCauseSpec; op=fun (e:enviro) -> e|>Ok}

    let fromCauseSpec (monitor:obj->unit)
                      (causeSpec:causeSpec) = 
     match causeSpec.genus with
     | [] -> "No CauseSpec genus" |> Error
     | ["NoOp"] -> noOp |> Ok
     | "Sorters"::b -> CauseSorters.fromCauseSpec b monitor causeSpec
     | "SorterShc"::b -> CauseSorterShc.fromCauseSpec b monitor causeSpec
     | "RandGen"::b -> CauseRandGen.fromCauseSpec b monitor causeSpec
     | a::b -> sprintf "Causes: %s not handled" a |> Error
     