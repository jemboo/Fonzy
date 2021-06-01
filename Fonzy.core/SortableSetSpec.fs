namespace global
open System

type SortableSetGenerated = { id:SortableSetId; 
                              cat:string; 
                              prams:Map<string, string>; }


type SortableSetSpec =
    | Explicit of SortableSet
    | Generated of SortableSetGenerated


module SortableSetSpec = 
    let getId (ss:SortableSetSpec) =
        match ss with
        | Explicit ess -> ess |> SortableSet.iD
        | Generated gss -> gss.id


