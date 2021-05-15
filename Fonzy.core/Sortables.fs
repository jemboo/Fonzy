namespace global
open System

type SortableSetBinary = {id:SortableSetId; degree:Degree; 
                            sortables:IntBits[]}
type SortableSetInteger = {id:SortableSetId; degree:Degree; 
                            sortables:int[][]}
type SortableSetBp32 = {id:SortableSetId; degree:Degree; 
                            sortables:bitsP32[]}
type SortableSetBp64 = {id:SortableSetId; degree:Degree; 
                            sortables:bitsP64[]}

type SortableSet =
     | Binary of SortableSetBinary
     | Integer of SortableSetInteger
     | Bp32 of SortableSetBp32
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


module SortableSetBp32 = 

    let allIntBitsId = SorterSetId.fromGuid (Guid.Parse "74000000-0000-0000-0000-000000000222")
    let rndBitsId =    SorterSetId.fromGuid (Guid.Parse "75000000-0000-0000-0000-000000000222")

    let allIntBits (degree:Degree) = 
        let id = seq { allIntBitsId:> obj; 
                       degree:> obj;} 
                        |> GuidUtils.guidFromObjs
        {
              SortableSetBp32.id = SortableSetId.fromGuid id;
              SortableSetBp32.degree = degree;
              SortableSetBp32.sortables = 
                        (bitsP32.arrayOfAllFor degree)
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
        let sias = bitsP32.createRandoms degree rando
                    |> Seq.take (SortableCount.value sortableCount)
                    |> Seq.toArray
        {
            SortableSetBp32.id = SortableSetId.fromGuid id;
            SortableSetBp32.degree = degree;
            SortableSetBp32.sortables = sias
        }