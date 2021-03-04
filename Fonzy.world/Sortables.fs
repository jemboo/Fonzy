namespace global
open System


module SortableSetGenerated =
    let rndBits (degree:Degree) (sortableCount:SortableCount) (rngGen:RngGen) = 
        let m = [("count", (SortableCount.value sortableCount).ToString()); 
                 ("degree", (Degree.value degree).ToString()); 
                 ("rngGen", rngGen |> RngGenDto.toJson )] |> Map.ofList
        let id = ([m :> obj]) |> GuidUtils.guidFromObjList
        {
            SortableSetGenerated.id = id ;
            SortableSetGenerated.cat = "rndBits";
            SortableSetGenerated.prams = m
        }