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

    let getIntSets (rep:sortableSetImpl) =
        match rep with
        | sortableSetImpl.Binary (bs, d) ->
            bs |> Array.map(BitSet.toIntSet)
        | sortableSetImpl.Integer (nts, d) -> nts
        | sortableSetImpl.Bp64 (bpS, d) -> 
            bpS |> BitsP64.toIntSets |> Seq.toArray



type sortableSetType = 
    | AllForDegree of sortableSetRep
    | Explicit of SortableSetId
    | Random of RngGen*SortableCount*sortableSetRep
    | SwitchReduced of sortableSetType*Switch list
    
type sortableSetMaker  = sortableSetType -> Result<sortableSetImpl,string>
type sortableSetMakerT  = sortableSetType -> Result<sortableSetImpl* switchUses, string>

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

    let makeT (sortableSetMakerT:sortableSetMakerT) 
             (sortableSetType:sortableSetType) =
        result {
            let! impl, uses = sortableSetMakerT sortableSetType
            let! ssId = seq { sortableSetType:> obj; } 
                         |> GuidUtils.guidFromObjs
                         |>  SortableSetId.create
            return ({
                sortableSet.id = ssId;
                sortableSetType = sortableSetType;
                sortableSetImpl = impl},
                uses)
        }