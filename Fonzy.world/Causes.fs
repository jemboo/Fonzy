namespace global
open System

module Causes =
    let noOp =
        {cause.causeSpec=CauseSpec.noOpCauseSpec; op=fun monitor (e:enviro) -> e |> Ok}

    let fromCauseSpec (causeSpec:causeSpec) = 
     match causeSpec.genus with
     | [] -> "No CauseSpec genus" |> Error
     | ["NoOp"] -> noOp |> Ok
     | "Sorters"::b -> CauseSorters.fromCauseSpec b causeSpec
     | "SorterShc"::b -> CauseSorterShc.fromCauseSpec b causeSpec
     | "RandGen"::b -> CauseRandGen.fromCauseSpec b causeSpec
     | a::b -> sprintf "Causes: %s not handled" a |> Error
     