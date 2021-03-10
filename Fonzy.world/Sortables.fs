namespace global
open System

type SortableSetExplicit = {id:Guid; degree:Degree; 
                            sortableIntArrays:SortableIntArray[]}
module SortableSetExplicit = 
    let toRollout (sse:SortableSetExplicit) =
        let sroll = sse.sortableIntArrays |> Array.map(SortableIntArray.value)
                                          |> Array.collect(id)
        {
            SortableSetRollout.degree = sse.degree;
            SortableSetRollout.sortableCount = SortableCount.fromInt 
                                                    sse.sortableIntArrays.Length;
            SortableSetRollout.baseArray = sroll
        }

type SortableSetGenerated = {id:Guid; cat:string; prams:Map<string, string>;}

module SortableSetGenerated =

    let allIntBits (degree:Degree) = 
        let m = [("degree", (Degree.value degree).ToString());]
                    |> Map.ofList
        let id = ([("allIntBits" :> obj); (m :> obj)]) |> GuidUtils.guidFromObjList
        {
            SortableSetGenerated.id = id ;
            SortableSetGenerated.cat = "allIntBits";
            SortableSetGenerated.prams = m
        }

    let rndBits (degree:Degree) (sortableCount:SortableCount) (rngGen:RngGen) = 
        let m = [("count", (SortableCount.value sortableCount).ToString()); 
                 ("degree", (Degree.value degree).ToString()); 
                 ("rngGen", rngGen |> RngGenDto.toJson )] |> Map.ofList
        let id = ([("rndBits" :> obj); (m :> obj)]) |> GuidUtils.guidFromObjList
        {
            SortableSetGenerated.id = id ;
            SortableSetGenerated.cat = "rndBits";
            SortableSetGenerated.prams = m
        }

    let rndPerms (degree:Degree) (sortableCount:SortableCount) (rngGen:RngGen) = 
        let m = [("count", (SortableCount.value sortableCount).ToString()); 
                 ("degree", (Degree.value degree).ToString()); 
                 ("rngGen", rngGen |> RngGenDto.toJson )] |> Map.ofList
        let id = ([("rndPerms" :> obj); (m :> obj)]) |> GuidUtils.guidFromObjList
        {
            SortableSetGenerated.id = id ;
            SortableSetGenerated.cat = "rndPerms";
            SortableSetGenerated.prams = m
        }



    let generate (ssg:SortableSetGenerated) = 
        match ssg.cat with
        | "rndBits" -> 
            result {
                      let! degree = ssg.prams |> ResultMap.procKeyedInt "degree" 
                                                           (fun d -> Degree.create "" d)

                      let sias = IntBits.AllBinaryTestCasesSeq (Degree.value degree)
                                    |> Seq.map(SortableIntArray.create)
                                    |> Seq.toArray
                      return {
                                SortableSetExplicit.id = ssg.id;
                                SortableSetExplicit.degree = degree;
                                SortableSetExplicit.sortableIntArrays = sias
                             }
                   }

        | "rndPerms" -> 
            result {
                      let! count = ssg.prams |> ResultMap.lookupKeyedInt "count"
                      let! degree = ssg.prams |> ResultMap.procKeyedInt "degree" 
                                                            (fun d -> Degree.create "" d)
                      let! rngGen = ssg.prams |> ResultMap.procKeyedString "rngGen" 
                                                                    (RngGenDto.fromJson)
                      let rando = rngGen |> Rando.fromRngGen

                      let sias = Permutation.createRandoms degree rando
                                    |> Seq.map(fun p -> SortableIntArray.create 
                                                         (Permutation.arrayValues p))
                                    |> Seq.take count
                                    |> Seq.toArray
                      return {
                                SortableSetExplicit.id = ssg.id;
                                SortableSetExplicit.degree = degree;
                                SortableSetExplicit.sortableIntArrays = sias
                             }
                   }

        | "allIntBits" -> 
            let makeSia (r:IRando) (d:Degree) =
                    SortableIntArray.create 
                        (ZeroOneSequence.Random r (Degree.value d) 0.5 |> Seq.toArray)
            result {
                        let! count = ssg.prams |> ResultMap.lookupKeyedInt "count"
                        let! degree = ssg.prams |> ResultMap.procKeyedInt "degree" 
                                                            (fun d -> Degree.create "" d)
                        let! rngGen = ssg.prams |> ResultMap.procKeyedString "rngGen" 
                                                                    (RngGenDto.fromJson)
                        let rando = rngGen |> Rando.fromRngGen
                        let sias = Array.init count (fun _ -> makeSia rando degree)
                        return {
                                SortableSetExplicit.id = ssg.id;
                                SortableSetExplicit.degree = degree;
                                SortableSetExplicit.sortableIntArrays = sias
                                }
                    }
        | _ -> Error (sprintf "no match for SortableSetGenerated.cat: %s" ssg.cat)



type SortableSet =
        | Explicit of SortableSetExplicit
        | Generated of SortableSetGenerated

module SortableSet = 
    let getId (ss:SortableSet) =
        match ss with
        | Explicit ess -> ess.id
        | Generated gss -> gss.id

    let getSortableSetExplicit (ss:SortableSet) =
        match ss with
        | Explicit ess -> ess |> Ok
        | Generated gss -> gss |> SortableSetGenerated.generate

