namespace global

module SortableSetGenerated =

    let allIntBits (degree:Degree) = 
        let m = [("degree", (Degree.value degree).ToString());]
                    |> Map.ofList
        let id = ([("allIntBits" :> obj); (m :> obj)]) |> GuidUtils.guidFromObjList
        {
            SortableSetGenerated.id = id |> SortableSetId.fromGuid;
            SortableSetGenerated.cat = "allIntBits";
            SortableSetGenerated.prams = m
        }

    let rndBits (id:SortableSetId) 
                (degree:Degree) 
                (sortableCount:SortableCount) 
                (rngGen:RngGen) = 
        let m = [("sortableCount", (SortableCount.value sortableCount).ToString()); 
                 ("id", (SortableSetId.value id) |> string); 
                 ("degree", (Degree.value degree).ToString()); 
                 ("rngGen", rngGen |> RngGenDto.toJson )] |> Map.ofList
        let id = ([("rndBits" :> obj); (m :> obj)]) |> GuidUtils.guidFromObjList
        {
            SortableSetGenerated.id = id |> SortableSetId.fromGuid;
            SortableSetGenerated.cat = "rndBits";
            SortableSetGenerated.prams = m
        }

    let rndPerms (degree:Degree) (sortableCount:SortableCount) (rngGen:RngGen) = 
        let m = [("count", (SortableCount.value sortableCount).ToString()); 
                 ("degree", (Degree.value degree).ToString()); 
                 ("rngGen", rngGen |> RngGenDto.toJson )] |> Map.ofList
        let id = ([("rndPerms" :> obj); (m :> obj)]) |> GuidUtils.guidFromObjList
        {
            SortableSetGenerated.id = id |> SortableSetId.fromGuid;
            SortableSetGenerated.cat = "rndPerms";
            SortableSetGenerated.prams = m
        }

    let generate (ssg:SortableSetGenerated) = 
        match ssg.cat with
        | "rndBits" -> 
            result {
                      let! degree = ssg.prams |> ResultMap.procKeyedInt "degree" 
                                                           (fun d -> Degree.create "" d)
                      let! rngGen = ssg.prams |> ResultMap.procKeyedString<RngGen> "rngGen" 
                                                           (RngGenDto.fromJson)
                      let! sortableCount = ssg.prams |> ResultMap.procKeyedInt "sortableCount" 
                                                           (SortableCount.create "")
                      return SortableSetExplicit.rndBits degree rngGen sortableCount
                   }

        | "rndPerms" -> 
            result {
                      let! count = ssg.prams |> ResultMap.procKeyedInt "count" 
                                                            (fun d -> SortableCount.create "" d)
                      let! degree = ssg.prams |> ResultMap.procKeyedInt "degree" 
                                                            (fun d -> Degree.create "" d)
                      let! rngGen = ssg.prams |> ResultMap.procKeyedString "rngGen" 
                                                                    (RngGenDto.fromJson)
                      return SortableSetExplicit.rndPerms degree rngGen count
                   }

        | "allIntBits" -> 
            result {
                        let! degree = ssg.prams |> ResultMap.procKeyedInt "degree" 
                                                            (fun d -> Degree.create "" d)

                        return SortableSetExplicit.allIntBits degree
                    }
        | _ -> Error (sprintf "no match for SortableSetGenerated.cat: %s" ssg.cat)


module SortableSet = 
    let getSortableSetExplicit (ss:SortableSet) =
        match ss with
        | Explicit ess -> ess |> Ok
        | Generated gss -> gss |> SortableSetGenerated.generate

