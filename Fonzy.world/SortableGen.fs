namespace global

module SortableSetGen =

    let allIntBits (degree:Degree) = 
        let m = [("degree", (Degree.value degree).ToString());]
                    |> Map.ofList
        let id = ([("allIntBits" :> obj); (m :> obj)]) |> GuidUtils.guidFromObjList
        {
            sortableSetGen.id = id |> SortableSetId.fromGuid;
            sortableSetGen.cat = "allIntBits";
            sortableSetGen.prams = m
        }

    let allBp64 (degree:Degree) = 
        let m = [("degree", (Degree.value degree).ToString());]
                    |> Map.ofList
        let id = ([("allBp64" :> obj); (m :> obj)]) |> GuidUtils.guidFromObjList
        {
            sortableSetGen.id = id |> SortableSetId.fromGuid;
            sortableSetGen.cat = "allBp64";
            sortableSetGen.prams = m
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
            sortableSetGen.id = id |> SortableSetId.fromGuid;
            sortableSetGen.cat = "rndBits";
            sortableSetGen.prams = m
        }

    //let rndPerms (degree:Degree) (sortableCount:SortableCount) (rngGen:RngGen) = 
    //    let m = [("count", (SortableCount.value sortableCount).ToString()); 
    //             ("degree", (Degree.value degree).ToString()); 
    //             ("rngGen", rngGen |> RngGenDto.toJson )] |> Map.ofList
    //    let id = ([("rndPerms" :> obj); (m :> obj)]) |> GuidUtils.guidFromObjList
    //    {
    //        SortableSetGenerated.id = id |> SortableSetId.fromGuid;
    //        SortableSetGenerated.cat = "rndPerms";
    //        SortableSetGenerated.prams = m
    //    }

    let generate (ssg:sortableSetGen) = 
        match ssg.cat with
        | "rndBits" -> 
            result {
                      let! degree = ssg.prams |> ResultMap.procKeyedInt "degree" 
                                                           (fun d -> Degree.create "" d)
                      let! rngGen = ssg.prams |> ResultMap.procKeyedString<RngGen> "rngGen" 
                                                           (RngGenDto.fromJson)
                      let! sortableCount = ssg.prams |> ResultMap.procKeyedInt "sortableCount" 
                                                           (SortableCount.create "")
                      return (SortableSetBinary.rndBits degree rngGen sortableCount) |> sortableSet.Binary
                   }

        //| "rndPerms" -> 
        //    result {
        //              let! count = ssg.prams |> ResultMap.procKeyedInt "count" 
        //                                                    (fun d -> SortableCount.create "" d)
        //              let! degree = ssg.prams |> ResultMap.procKeyedInt "degree" 
        //                                                    (fun d -> Degree.create "" d)
        //              let! rngGen = ssg.prams |> ResultMap.procKeyedString "rngGen" 
        //                                                            (RngGenDto.fromJson)
        //              return SortableSetInteger.rndPerms degree rngGen count
        //           }

        | "allIntBits" -> 
            result {
                        let! degree = ssg.prams |> ResultMap.procKeyedInt "degree" 
                                                            (fun d -> Degree.create "" d)

                        return  ( SortableSetBinary.allIntBits degree ) |> sortableSet.Binary
                    }

        | "allBp64" -> 
            result {
                        let! degree = ssg.prams |> ResultMap.procKeyedInt "degree" 
                                                            (fun d -> Degree.create "" d)

                        return ( SortableSetBp64.allBp64 degree ) |> sortableSet.Bp64
                    }


        | _ -> Error (sprintf "no match for SortableSetGenerated.cat: %s" ssg.cat)


module SortableSetSpec = 
    let getSortableSet (ss:sortableSetSpec) =
        match ss with
        | Explicit ess -> ess |> Ok
        | Generated gss -> gss |> SortableSetGen.generate


module SortableSetSpecReduced = 
    let make (spec:sortableSetSpecReduced) = 
        result {
            let sSetSpec, switchPfx = spec
            let! sSet = sSetSpec |> SortableSetSpec.getSortableSet
            let ssRdx, pfxUses = SortingOps.SortableSet.switchReduce
                                        sSet
                                        switchPfx
            return ssRdx, pfxUses
        }
