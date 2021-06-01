namespace global
open System

type SortableSetBinary = {id:SortableSetId; degree:Degree; 
                            sortables:IntBits[]}
type SortableSetInteger = {id:SortableSetId; degree:Degree; 
                            sortables:int[][]}
type SortableSetBp64 = {id:SortableSetId; degree:Degree; 
                            sortables:bitsP64[]}

type SortableSet =
     | Binary of SortableSetBinary
     | Integer of SortableSetInteger
     | Bp64 of SortableSetBp64


module SortableSetBinary = 
    let allIntBitsId = SorterSetId.fromGuid (Guid.Parse "71000000-0000-0000-0000-000000000222")
    let rndBitsId =    SorterSetId.fromGuid (Guid.Parse "72000000-0000-0000-0000-000000000222")

    let allIntBits (degree:Degree) = 
        let id = seq { allIntBitsId:> obj; 
                       degree:> obj;} 
                        |> GuidUtils.guidFromObjs
        {
              SortableSetBinary.id = SortableSetId.fromGuid id;
              SortableSetBinary.degree = degree;
              SortableSetBinary.sortables = 
                        (IntBits.arrayOfAllFor degree)
                        |> Seq.toArray
        }


    let allBp64 (degree:Degree) = 
        let id = seq { allIntBitsId:> obj; 
                       degree:> obj;} 
                        |> GuidUtils.guidFromObjs
        {
              SortableSetBp64.id = SortableSetId.fromGuid id;
              SortableSetBp64.degree = degree;
              SortableSetBp64.sortables = 
                        (BitsP64.arrayOfAllFor degree)
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
            SortableSetBinary.id = SortableSetId.fromGuid id;
            SortableSetBinary.degree = degree;
            SortableSetBinary.sortables = sias
        }
    

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
              SortableSetInteger.id = SortableSetId.fromGuid id;
              SortableSetInteger.degree = degree;
              SortableSetInteger.sortables = sia
        }
     

module SortableSetBp64 = 

    let allIntBitsId = SorterSetId.fromGuid (Guid.Parse "74000000-0000-0000-0000-000000000222")
    let rndBitsId =    SorterSetId.fromGuid (Guid.Parse "75000000-0000-0000-0000-000000000222")

    let allIntBits (degree:Degree) = 
        let id = seq { allIntBitsId:> obj; 
                       degree:> obj;} 
                        |> GuidUtils.guidFromObjs
        {
              SortableSetBp64.id = SortableSetId.fromGuid id;
              SortableSetBp64.degree = degree;
              SortableSetBp64.sortables = 
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
            SortableSetBp64.id = SortableSetId.fromGuid id;
            SortableSetBp64.degree = degree;
            SortableSetBp64.sortables = sias
        }


module SortableSet = 
    let iD (ss:SortableSet) =
        match ss with
        | Binary  ss -> ss.id
        | Integer  ss -> ss.id
        | Bp64 ss -> ss.id