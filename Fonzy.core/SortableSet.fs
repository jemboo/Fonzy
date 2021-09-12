namespace global
open System

type sortableSetRep =
    | Binary of Degree
    | Integer of Degree
    | Bp64 of Degree

module SortableSetRep =
    let getDegree (rep:sortableSetRep) =
        match rep with
        | sortableSetRep.Binary d -> d
        | sortableSetRep.Integer d -> d
        | sortableSetRep.Bp64 d -> d

type sortableSetImpl =
    | Binary of bitSet[] * Degree
    | Integer of intSet[] * Degree
    | Bp64 of bitsP64[] * Degree

module SortableSetImpl =
    let getDegree (rep:sortableSetImpl) =
        match rep with
        | sortableSetImpl.Binary (bs, d) -> d
        | sortableSetImpl.Integer (nts, d) -> d
        | sortableSetImpl.Bp64 (bpS, d) -> d

    //let getId (rep:sortableSetImpl) =
    //    match rep with
    //    | sortableSetImpl.Binary (bs, d, ssid) -> ssid
    //    | sortableSetImpl.Integer (nts, d, ssid) -> ssid
    //    | sortableSetImpl.Bp64 (bpS, d, ssid) -> ssid


type sortableSetType = 
    | AllForDegree of sortableSetRep
    | Explicit of SortableSetId
    | Random of RngGen*SortableCount*sortableSetRep
    | SwitchReduced of sortableSetType*Switch list
    
type sortableSetMaker  = sortableSetType -> Result<sortableSetImpl,string>

type sortableSet = { id:SortableSetId;
                     sortableSetType:sortableSetType; 
                     sortableSetImpl:sortableSetImpl; }


module SortableSet = 
    let make (sortableSetMaker:sortableSetMaker) 
             (sortableSetType:sortableSetType) =
        result {
            let! rep = sortableSetMaker sortableSetType
            let! ssId = seq { sortableSetType:> obj; } 
                         |> GuidUtils.guidFromObjs
                         |>  SortableSetId.create
            return {
                sortableSet.id = ssId;
                sortableSetType = sortableSetType;
                sortableSetImpl = rep}
        }

            


//type sortableSetBinary = {id:SortableSetId; degree:Degree; 
//                            sortables:bitSet[]}
//type sortableSetInteger = {id:SortableSetId; degree:Degree; 
//                            sortables:intSet[]}
//type sortableSetBp64 = {id:SortableSetId; degree:Degree; 
//                            sortables:bitsP64[]}

//type sortableSetO =
//     | Binary of sortableSetBinary
//     | Integer of sortableSetInteger
//     | Bp64 of sortableSetBp64


//module SortableSetBinary = 
//    let allIntBitsId = SorterSetId.fromGuid (Guid.Parse "71000000-0000-0000-0000-000000000222")
//    let rndBitsId =    SorterSetId.fromGuid (Guid.Parse "72000000-0000-0000-0000-000000000222")

//    let allIntBits (degree:Degree) = 
//        let id = seq { allIntBitsId:> obj; 
//                       degree:> obj;} 
//                 |> GuidUtils.guidFromObjs
//        {
//              sortableSetBinary.id = SortableSetId.fromGuid id;
//              degree = degree;
//              sortables = (BitSet.arrayOfAllFor degree)
//                          |> Seq.toArray
//        }

//    let rndBits (degree:Degree) 
//                (rngGen:RngGen) 
//                (sortableCount:SortableCount) = 
//        let id = seq { rndBitsId:> obj;
//                       degree:> obj;
//                       rngGen:> obj; 
//                       sortableCount:> obj;} 
//                 |> GuidUtils.guidFromObjs
//        let rando = rngGen |> Rando.fromRngGen
//        let sias = BitSet.createRandoms degree rando
//                    |> Seq.take (SortableCount.value sortableCount)
//                    |> Seq.toArray
//        {
//            sortableSetBinary.id = SortableSetId.fromGuid id;
//            degree = degree;
//            sortables = sias
//        }

//    let fromBitSet (degree:Degree) (bitSets:bitSet[]) = 
//        let id = seq { bitSets:> obj; 
//                       degree:> obj;} 
//                        |> GuidUtils.guidFromObjs
//        {
//              sortableSetBinary.id = SortableSetId.fromGuid id;
//              degree = degree;
//              sortables = bitSets
//        }
    
//    let toBitSet (ssBin:sortableSetBinary) = 
//        ssBin.sortables


//module SortableSetInteger = 

//    let rndPermsId = SorterSetId.fromGuid (Guid.Parse "73000000-0000-0000-0000-000000000222")

//    let rndPerms (degree:Degree) 
//                 (rngGen:RngGen) 
//                 (sortableCount:SortableCount) = 
//        let id = seq { rndPermsId:> obj;
//                       degree:> obj;
//                       rngGen:> obj;
//                       sortableCount:> obj;} 
//                  |> GuidUtils.guidFromObjs
//        let rando = rngGen |> Rando.fromRngGen

//        let sia = Permutation.createRandoms degree rando
//                    |> Seq.map(fun p -> IntSet.create (Permutation.arrayValues p))
//                    |> Seq.take (SortableCount.value sortableCount)
//                    |> Seq.toArray
//        {
//              sortableSetInteger.id = SortableSetId.fromGuid id;
//              degree = degree;
//              sortables = sia
//        }

//    let fromIntSet (degree:Degree) 
//                   (intSets:intSet[]) = 
//        let id = seq { intSets :> obj; degree:> obj;} 
//                 |> GuidUtils.guidFromObjs
//        {
//              sortableSetInteger.id = SortableSetId.fromGuid id;
//              degree = degree;
//              sortables = intSets
//        }
     
//    let toBitSet (ssInt:sortableSetInteger) = 
//        ssInt.sortables |> Seq.map(fun aa -> BitSet.create aa.values)


//module SortableSetBp64 = 

//    let allIntBitsId = SorterSetId.fromGuid (Guid.Parse "74000000-0000-0000-0000-000000000222")
//    let rndBitsId =    SorterSetId.fromGuid (Guid.Parse "75000000-0000-0000-0000-000000000222")

//    let allBp64 (degree:Degree) = 
//        let id = seq { allIntBitsId:> obj; 
//                       degree:> obj;} 
//                        |> GuidUtils.guidFromObjs
//        {
//              sortableSetBp64.id = SortableSetId.fromGuid id;
//              degree = degree;
//              sortables = 
//                        (BitsP64.arrayOfAllFor degree)
//                        |> Seq.toArray
//         }
    

//    let rndBits (degree:Degree) 
//                (rngGen:RngGen) 
//                (sortableCount:SortableCount) = 
//        let id = seq { rndBitsId:> obj;
//                       degree:> obj;
//                       rngGen:> obj; 
//                       sortableCount:> obj;  } 
//                  |> GuidUtils.guidFromObjs

//        let rando = rngGen |> Rando.fromRngGen
//        let sias = BitsP64.createRandoms  degree 
//                                          rando
//                                          (SortableCount.value sortableCount)
//                    |> Seq.toArray
//        {
//            sortableSetBp64.id = SortableSetId.fromGuid id;
//            degree = degree;
//            sortables = sias
//        }


//    let fromBitSet (degree:Degree) 
//                    (intBits: bitSet[]) = 
//        let id = seq { intBits:> obj; 
//                       degree:> obj;} 
//                        |> GuidUtils.guidFromObjs
//        {
//              sortableSetBp64.id = SortableSetId.fromGuid id;
//              degree = degree;
//              sortables = (BitsP64.fromBitSet intBits)
//                           |> Seq.toArray
//        }


//    let toBitSet (ssBp64:sortableSetBp64) = 
//        ssBp64.sortables |> BitsP64.toBitSet



//module SortableSet0 = 
//    let iD (ss:sortableSetO) =
//        match ss with
//        | Binary  ss -> ss.id
//        | Integer  ss -> ss.id
//        | Bp64 ss -> ss.id


//    let degree (ss:sortableSetO) =
//        match ss with
//        | Binary  ss -> ss.degree
//        | Integer  ss -> ss.degree
//        | Bp64 ss -> ss.degree

//    let toIntBits (ss:sortableSetO) = 
//        match ss with
//        | Binary  ss -> ss |> SortableSetBinary.toBitSet |> Array.toSeq
//        | Integer  ss -> ss |> SortableSetInteger.toBitSet
//        | Bp64 ss -> ss |> SortableSetBp64.toBitSet



type sortableSetGen = 
    { 
        id:SortableSetId; 
        cat:string; 
        prams:Map<string, string>; 
    }


//type sortableSetSpec =
//    | Explicit of sortableSetO
//    | Generated of sortableSetGen

//type sortableSetSpecReduced = sortableSetSpec*Switch[]


//module SortableSetSpec = 
//    let getId (ss:sortableSetSpec) =
//        match ss with
//        | Explicit ess -> ess |> SortableSet0.iD
//        | Generated gss -> gss.id