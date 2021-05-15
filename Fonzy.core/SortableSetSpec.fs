namespace global
open System


type SortableSetGenerated = {id:SortableSetId; cat:string; prams:Map<string, string>;}

type SortableSetSpec =
    | Explicit of SortableSetBinary
    | Generated of SortableSetGenerated

module SortableSetSpec = 
    let getId (ss:SortableSetSpec) =
        match ss with
        | Explicit ess -> ess.id
        | Generated gss -> gss.id


