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
    | Binary of intSetsRollout
    | Integer of intSetsRollout
    | Bp64 of bP64SetsRollout


module SortableSetImpl =
    let getDegree (rep:sortableSetImpl) =
        match rep with
        | sortableSetImpl.Binary isr -> isr.degree
        | sortableSetImpl.Integer isr -> isr.degree
        | sortableSetImpl.Bp64 bsr -> bsr.degree

    let getIntSets (rep:sortableSetImpl) =
        match rep with
        | sortableSetImpl.Binary isr ->
            isr |> IntSetsRollout.toIntSet
        | sortableSetImpl.Integer isr ->
            isr |> IntSetsRollout.toIntSet
        | sortableSetImpl.Bp64 bsr ->
            bsr |> BP64SetsRollout.toIntSet

    let toSortableSetRollout (impl:sortableSetImpl) = 
        match impl with
        | sortableSetImpl.Binary isr ->
            result {
             return isr |> sortableSetRollout.Int
            }
        | sortableSetImpl.Integer isr ->
            result {
             return isr |> sortableSetRollout.Int
            }
        
        | sortableSetImpl.Bp64 bsr ->
            result {
             return bsr |> sortableSetRollout.Bp64
            }


type sortableSetType = 
    | BinaryMerge of (Degree list)*sortableSetRep
    | AllForDegree of sortableSetRep
    | Explicit of SortableSetId
    | Random of RngGen*SortableCount*sortableSetRep
    | SwitchReduced of sortableSetType*Switch list

module SortableSetType = 
    let getPrefix sst =
        match sst with
        | sortableSetType.BinaryMerge (dlst, ssr) -> []
        | sortableSetType.AllForDegree ssr -> []
        | sortableSetType.Explicit  ssid -> []
        | sortableSetType.Random (r, sc, ssr) -> []
        | sortableSetType.SwitchReduced (sst, swL) -> swL
    
type sortableSetMaker  = sortableSetType -> Result<sortableSetImpl,string>
type sortableSetMakerT  = sortableSetType -> Result<sortableSetImpl* switchUses, string>

type sortableSet = { id:SortableSetId;
                     sortableSetType:sortableSetType; 
                     sortableSetImpl:sortableSetImpl; }


module SortableSet = 
    let make (sortableSetMaker:sortableSetMaker) 
             (sortableSetType:sortableSetType) =
        result {
            let! impl = sortableSetMaker sortableSetType
            let! ssId = seq { sortableSetType:> obj; } 
                         |> GuidUtils.guidFromObjs
                         |>  SortableSetId.create
            return {
                sortableSet.id = ssId;
                sortableSetType = sortableSetType;
                sortableSetImpl = impl}
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