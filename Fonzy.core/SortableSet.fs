namespace global
open System

type sortableSetBinary = {id:SortableSetId; degree:Degree; 
                            sortables:IntBits[]}
type sortableSetInteger = {id:SortableSetId; degree:Degree; 
                            sortables:int[][]}
type sortableSetBp64 = {id:SortableSetId; degree:Degree; 
                            sortables:bitsP64[]}

type sortableSet =
     | Binary of sortableSetBinary
     | Integer of sortableSetInteger
     | Bp64 of sortableSetBp64


module SortableSetBinary = 
    let allIntBitsId = SorterSetId.fromGuid (Guid.Parse "71000000-0000-0000-0000-000000000222")
    let rndBitsId =    SorterSetId.fromGuid (Guid.Parse "72000000-0000-0000-0000-000000000222")

    let allIntBits (degree:Degree) = 
        let id = seq { allIntBitsId:> obj; 
                       degree:> obj;} 
                        |> GuidUtils.guidFromObjs
        {
              sortableSetBinary.id = SortableSetId.fromGuid id;
              degree = degree;
              sortables = (IntBits.arrayOfAllFor degree)
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
                    |> Seq.toArray
        {
            sortableSetBinary.id = SortableSetId.fromGuid id;
            degree = degree;
            sortables = sias
        }

    let fromIntBits (degree:Degree) (intBits: IntBits[]) = 
        let id = seq { intBits:> obj; 
                       degree:> obj;} 
                        |> GuidUtils.guidFromObjs
        {
              sortableSetBinary.id = SortableSetId.fromGuid id;
              degree = degree;
              sortables = intBits
        }
    
    let toIntBits (ssBin:sortableSetBinary) = 
        ssBin.sortables


module SortableSetInteger = 

    let rndPermsId = SorterSetId.fromGuid (Guid.Parse "73000000-0000-0000-0000-000000000222")

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
                    |> Seq.map(fun p -> Permutation.arrayValues p)
                    |> Seq.take (SortableCount.value sortableCount)
                    |> Seq.toArray
        {
              sortableSetInteger.id = SortableSetId.fromGuid id;
              degree = degree;
              sortables = sia
        }

    let fromIntBits (degree:Degree) (intBits: IntBits[]) = 
        let id = seq { intBits:> obj; 
                       degree:> obj;} 
                        |> GuidUtils.guidFromObjs
        {
              sortableSetInteger.id = SortableSetId.fromGuid id;
              degree = degree;
              sortables = intBits
                |> Array.map(fun ib -> ib.values)
         }
     
    let toIntBits (ssInt:sortableSetInteger) = 
        ssInt.sortables |> Seq.map(fun aa -> IntBits.create aa)


module SortableSetBp64 = 

    let allIntBitsId = SorterSetId.fromGuid (Guid.Parse "74000000-0000-0000-0000-000000000222")
    let rndBitsId =    SorterSetId.fromGuid (Guid.Parse "75000000-0000-0000-0000-000000000222")

    let allBp64 (degree:Degree) = 
        let id = seq { allIntBitsId:> obj; 
                       degree:> obj;} 
                        |> GuidUtils.guidFromObjs
        {
              sortableSetBp64.id = SortableSetId.fromGuid id;
              degree = degree;
              sortables = 
                        (BitsP64.arrayOfAllFor degree)
                        |> Seq.toArray
         }
    

    let rndBits (degree:Degree) 
                (rngGen:RngGen) 
                (sortableCount:SortableCount) = 
        let id = seq { rndBitsId:> obj;
                       degree:> obj;
                       rngGen:> obj; 
                       sortableCount:> obj;  } 
                  |> GuidUtils.guidFromObjs

        let rando = rngGen |> Rando.fromRngGen
        let sias = BitsP64.createRandoms  degree 
                                          rando
                                          (SortableCount.value sortableCount)
                    |> Seq.toArray
        {
            sortableSetBp64.id = SortableSetId.fromGuid id;
            degree = degree;
            sortables = sias
        }


    let fromIntBits (degree:Degree) 
                    (intBits: IntBits[]) = 
        let id = seq { intBits:> obj; 
                       degree:> obj;} 
                        |> GuidUtils.guidFromObjs
        {
              sortableSetBp64.id = SortableSetId.fromGuid id;
              degree = degree;
              sortables = (BitsP64.fromIntBits intBits)
                           |> Seq.toArray
         }


    let toIntBits (ssBp64:sortableSetBp64) = 
        ssBp64.sortables |> BitsP64.toIntBits



module SortableSet = 
    let iD (ss:sortableSet) =
        match ss with
        | Binary  ss -> ss.id
        | Integer  ss -> ss.id
        | Bp64 ss -> ss.id


    let degree (ss:sortableSet) =
        match ss with
        | Binary  ss -> ss.degree
        | Integer  ss -> ss.degree
        | Bp64 ss -> ss.degree

    let toIntBits (ss:sortableSet) = 
        match ss with
        | Binary  ss -> ss |> SortableSetBinary.toIntBits |> Array.toSeq
        | Integer  ss -> ss |> SortableSetInteger.toIntBits
        | Bp64 ss -> ss |> SortableSetBp64.toIntBits



type sortableSetGen = 
    { 
        id:SortableSetId; 
        cat:string; 
        prams:Map<string, string>; 
    }


type sortableSetSpec =
    | Explicit of sortableSet
    | Generated of sortableSetGen


type sortableSetSpecReduced = sortableSetSpec*Switch[]


module SortableSetSpec = 
    let getId (ss:sortableSetSpec) =
        match ss with
        | Explicit ess -> ess |> SortableSet.iD
        | Generated gss -> gss.id