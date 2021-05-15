namespace global
open System


//type SortableIntArray = private SortableIntArray of int[]
//module SortableIntArray =
//    let create (intArray:int[]) = SortableIntArray intArray


type SortableSetExplicit = {id:SortableSetId; degree:Degree; 
                            sortableIntArrays:IntBits[]}

type SortableSetGenerated = {id:SortableSetId; cat:string; prams:Map<string, string>;}


type SortableSet =
        | Explicit of SortableSetExplicit
        | Generated of SortableSetGenerated

module SortableSet = 
    let getId (ss:SortableSet) =
        match ss with
        | Explicit ess -> ess.id
        | Generated gss -> gss.id


module SortableSetExplicit = 
    let allIntBitsId = SorterSetId.fromGuid (Guid.Parse "71000000-0000-0000-0000-000000000222")
    let rndBitsId =    SorterSetId.fromGuid (Guid.Parse "72000000-0000-0000-0000-000000000222")
    let rndPermsId = SorterSetId.fromGuid (Guid.Parse "73000000-0000-0000-0000-000000000222")

    let allIntBits (degree:Degree) = 
        let id = seq { allIntBitsId:> obj; 
                       degree:> obj;} 
                        |> GuidUtils.guidFromObjs
        {
              SortableSetExplicit.id = SortableSetId.fromGuid id;
              SortableSetExplicit.degree = degree;
              SortableSetExplicit.sortableIntArrays = 
                        (IntBits.arrayOfAllFor degree)
                        |> Seq.toArray
         }

    let rndBits (degree:Degree) 
                (rngGen:RngGen) 
                (sortableCount:SortableCount) = 
        let id = seq { rndBitsId:> obj;
                       degree:> obj;
                       rngGen:> obj; 
                       sortableCount:> obj;} 
                        |> GuidUtils.guidFromObjs
        let rando = rngGen |> Rando.fromRngGen
        let sias = IntBits.createRandoms degree rando
                    |> Seq.take (SortableCount.value sortableCount)
                    |> Seq.map(fun p -> {IntBits.values = p.values})
                    |> Seq.toArray
        {
            SortableSetExplicit.id = SortableSetId.fromGuid id;
            SortableSetExplicit.degree = degree;
            SortableSetExplicit.sortableIntArrays = sias
        }

    let rndPerms (degree:Degree) 
                 (rngGen:RngGen) 
                 (sortableCount:SortableCount) = 
        let id = seq { rndPermsId:> obj;
                       degree:> obj;
                       rngGen:> obj;
                       sortableCount:> obj;} 
                        |> GuidUtils.guidFromObjs
        let rando = rngGen |> Rando.fromRngGen
        let sia = Permutation.createRandoms degree rando
                    |> Seq.map(fun p -> { IntBits.values = 
                                            (Permutation.arrayValues p)})
                    |> Seq.take (SortableCount.value sortableCount)
                    |> Seq.toArray
        {
              SortableSetExplicit.id = SortableSetId.fromGuid id;
              SortableSetExplicit.degree = degree;
              SortableSetExplicit.sortableIntArrays = sia
         }



    //let toRollout (sse:SortableSetExplicit) =
    //    let sroll = sse.sortableIntArrays |> Array.map(IntBits.value)
    //                                      |> Array.collect(id)
    //    {
    //        SortableSetRollout.degree = sse.degree;
    //        SortableSetRollout.sortableCount = SortableCount.fromInt 
    //                                                sse.sortableIntArrays.Length;
    //        SortableSetRollout.baseArray = sroll
    //    }
