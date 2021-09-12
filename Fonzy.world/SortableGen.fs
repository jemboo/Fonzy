namespace global

module SortableSetMaker = 
    let rndIntBits (ssr:sortableSetRep) 
                   (degree:Degree) 
                   (rngGen:RngGen) 
                   (sortableCount:SortableCount) = 
        let rando = rngGen |> Rando.fromRngGen
        let intSet() = 
            IntSet.createRandoms degree 
                                 rando
            |> Seq.take (SortableCount.value sortableCount)
            |> Seq.toArray

        let bitSet() = 
            BitSet.createRandoms degree 
                                 rando
            |> Seq.take (SortableCount.value sortableCount)
            |> Seq.toArray

        let bitsP64Set() = 
            BitsP64.createRandoms degree 
                                  rando 
                                  (SortableCount.value sortableCount)
            |> Seq.toArray

        match ssr with
        | sortableSetRep.Binary d -> (bitSet(), d) |> sortableSetImpl.Binary
        | sortableSetRep.Integer d -> (intSet(), d) |> sortableSetImpl.Integer
        | sortableSetRep.Bp64 d -> (bitsP64Set(), d) |> sortableSetImpl.Bp64


    let allZeroOnes (ssr:sortableSetRep) = 
        let degree = (ssr |> SortableSetRep.getDegree)
        
        let ssid = ([("allZeroOnes" :> obj); (degree :> obj)]) 
                   |> GuidUtils.guidFromObjList
                   |> SortableSetId.fromGuid
        let intSet() = 
            IntSet.arrayOfAllFor degree
            |> Seq.toArray

        let bitSet() = 
            (BitSet.arrayOfAllFor degree)
            |> Seq.toArray

        let bitsP64Set() = 
            (BitsP64.arrayOfAllFor degree)
            |> Seq.toArray

        match ssr with
        | sortableSetRep.Binary d -> 
            (bitSet(), d) |> sortableSetImpl.Binary
        | sortableSetRep.Integer d -> 
            (intSet(), d) |> sortableSetImpl.Integer
        | sortableSetRep.Bp64 d -> 
            (bitsP64Set(), d) |> sortableSetImpl.Bp64



    let rec makeT (ssType:sortableSetType) = 
        match ssType with
        | sortableSetType.AllForDegree ssr -> 
                (allZeroOnes ssr, {switchUses.weights =[||] }) |> Ok
        | sortableSetType.Explicit ssid ->
                "sortableSetType.Explicit is Not supported" |> Error
        | sortableSetType.Random (rng, sc, ssr) ->
            let degree = ssr |> SortableSetRep.getDegree
            (rndIntBits ssr degree rng sc, {switchUses.weights =[||] })  |> Ok
        | sortableSetType.SwitchReduced (sst, swPfx) ->
            result {
                let! toChop = makeT sst
                let! res = SortingOps.SortableSet.switchReduce2
                                            (fst toChop)
                                            (swPfx |> List.toArray)
                return res
            }

    let rec make (repo: (SortableSetId->sortableSetImpl) option) 
                 (ssType:sortableSetType) = 
        let _lookup ssid =
            match repo with
            | Some f -> f ssid |> Ok
            | None -> "repo missing" |> Error
        match ssType with
        | sortableSetType.AllForDegree ssr -> 
                allZeroOnes ssr |> Ok
        | sortableSetType.Explicit ssid ->
                _lookup ssid
        | sortableSetType.Random (rng, sc, ssr) ->
            let degree = ssr |> SortableSetRep.getDegree
            rndIntBits ssr degree rng sc  |> Ok
        | sortableSetType.SwitchReduced (sst, swPfx) ->
            result {
                let! toChop = make repo sst
                let! res = SortingOps.SortableSet.switchReduce2
                                            toChop
                                            (swPfx |> List.toArray)
                return fst res
            }

    let makeNoRepo (sortableSetType:sortableSetType) =
        SortableSet.make (make None) sortableSetType

    let makeMemoize (repo: (SortableSetId->sortableSetImpl) option)  =
        FuncUtils.memoization (make repo)


//module SortableSetGen =

//    let allIntBits (degree:Degree) = 
//        let m = [("degree", (Degree.value degree).ToString());]
//                    |> Map.ofList
//        let id = ([("allIntBits" :> obj); (m :> obj)]) |> GuidUtils.guidFromObjList
//        {
//            sortableSetGen.id = id |> SortableSetId.fromGuid;
//            sortableSetGen.cat = "allIntBits";
//            sortableSetGen.prams = m
//        }

//    let allBp64 (degree:Degree) = 
//        let m = [("degree", (Degree.value degree).ToString());]
//                    |> Map.ofList
//        let id = ([("allBp64" :> obj); (m :> obj)]) |> GuidUtils.guidFromObjList
//        {
//            sortableSetGen.id = id |> SortableSetId.fromGuid;
//            sortableSetGen.cat = "allBp64";
//            sortableSetGen.prams = m
//        }

//    let rndBits (id:SortableSetId) 
//                (degree:Degree) 
//                (sortableCount:SortableCount) 
//                (rngGen:RngGen) = 
//        let m = [("sortableCount", (SortableCount.value sortableCount).ToString()); 
//                 ("id", (SortableSetId.value id) |> string); 
//                 ("degree", (Degree.value degree).ToString()); 
//                 ("rngGen", rngGen |> RngGenDto.toJson )] |> Map.ofList
//        let id = ([("rndBits" :> obj); (m :> obj)]) |> GuidUtils.guidFromObjList
//        {
//            sortableSetGen.id = id |> SortableSetId.fromGuid;
//            sortableSetGen.cat = "rndBits";
//            sortableSetGen.prams = m
//        }

//    //let rndPerms (degree:Degree) (sortableCount:SortableCount) (rngGen:RngGen) = 
//    //    let m = [("count", (SortableCount.value sortableCount).ToString()); 
//    //             ("degree", (Degree.value degree).ToString()); 
//    //             ("rngGen", rngGen |> RngGenDto.toJson )] |> Map.ofList
//    //    let id = ([("rndPerms" :> obj); (m :> obj)]) |> GuidUtils.guidFromObjList
//    //    {
//    //        SortableSetGenerated.id = id |> SortableSetId.fromGuid;
//    //        SortableSetGenerated.cat = "rndPerms";
//    //        SortableSetGenerated.prams = m
//    //    }

//    let generate (ssg:sortableSetGen) = 
//        match ssg.cat with
//        | "rndBits" -> 
//            result {
//                      let! degree = ssg.prams |> ResultMap.procKeyedInt "degree" 
//                                                           (fun d -> Degree.create "" d)
//                      let! rngGen = ssg.prams |> ResultMap.procKeyedString<RngGen> "rngGen" 
//                                                           (RngGenDto.fromJson)
//                      let! sortableCount = ssg.prams |> ResultMap.procKeyedInt "sortableCount" 
//                                                           (SortableCount.create "")
//                      return (SortableSetBinary.rndBits degree rngGen sortableCount) 
//                              |> sortableSetO.Binary
//                   }

//        //| "rndPerms" -> 
//        //    result {
//        //              let! count = ssg.prams |> ResultMap.procKeyedInt "count" 
//        //                                                    (fun d -> SortableCount.create "" d)
//        //              let! degree = ssg.prams |> ResultMap.procKeyedInt "degree" 
//        //                                                    (fun d -> Degree.create "" d)
//        //              let! rngGen = ssg.prams |> ResultMap.procKeyedString "rngGen" 
//        //                                                            (RngGenDto.fromJson)
//        //              return SortableSetInteger.rndPerms degree rngGen count
//        //           }

//        | "allIntBits" -> 
//            result {
//                        let! degree = ssg.prams |> ResultMap.procKeyedInt "degree" 
//                                                            (fun d -> Degree.create "" d)

//                        return  ( SortableSetBinary.allIntBits degree ) |> sortableSetO.Binary
//                    }

//        | "allBp64" -> 
//            result {
//                        let! degree = ssg.prams |> ResultMap.procKeyedInt "degree" 
//                                                            (fun d -> Degree.create "" d)

//                        return ( SortableSetBp64.allBp64 degree ) |> sortableSetO.Bp64
//                    }


//        | _ -> Error (sprintf "no match for SortableSetGenerated.cat: %s" ssg.cat)


//module SortableSetSpec = 
//    let getSortableSet (ss:sortableSetSpec) =
//        match ss with
//        | Explicit ess -> ess |> Ok
//        | Generated gss -> gss |> SortableSetGen.generate


//module SortableSetSpecReduced = 
//    let make (spec:sortableSetSpecReduced) = 
//        result {
//            let sSetSpec, switchPfx = spec
//            let! sSet = sSetSpec |> SortableSetSpec.getSortableSet
//            let! ssRdx, pfxUses = SortingOps.SortableSet.switchReduce
//                                        sSet
//                                        switchPfx
//            return ssRdx, pfxUses
//        }

//    let make2 (sortables:sortableSetImpl) 
//              (pfxs:Switch[]) = 
//        None

    //let makeMemoize =
    //    FuncUtils.memoization make