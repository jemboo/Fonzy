namespace global
open System

type MapOfOrgAttributes<'attr> = {
    id:Guid;
    attrMap:Map<OrgId, 'attr>}


module MapOfOrgAttributes = 
    let create<'attr> 
            (poolId:Guid) 
            (ids:seq<OrgId>)
            (attrs: seq<'attr>) =
        let attrMap = attrs |> Seq.zip ids |> Map.ofSeq
        {
            MapOfOrgAttributes.id=poolId;
            attrMap=attrMap;
        }

    let createUniform<'attr> 
            (poolId:Guid) 
            (ids:seq<OrgId>)
            (attrF: unit->'attr) =
        let attrMap = ids |> Seq.map(fun id->(id, attrF())) |> Map.ofSeq
        {
            MapOfOrgAttributes.id=poolId;
            attrMap=attrMap;
        }

    let makeMapOfAttributes<'attr> 
            (poolId:Guid) 
            (ids:seq<OrgId>)
            (locator:OrgId->'attr) = 
        create 
            poolId 
            ids
            (ids |> Seq.map locator)

