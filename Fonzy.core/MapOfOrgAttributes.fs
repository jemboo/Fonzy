namespace global
open System

type MapOfOrgAttributes<'attr> = {
    id:Guid;
    orgAttributeType:OrgAttributeType;
    attrMap:Map<OrgId, 'attr>}


module MapOfOrgAttributes = 
    let create<'attr> 
            (poolId:Guid) 
            (ids:seq<OrgId>)
            (orgAttributeType:OrgAttributeType)
            (attrs: seq<'attr>) =
        let attrMap = attrs |> Seq.zip ids  |> Map.ofSeq
        {
            MapOfOrgAttributes.id=poolId;
            orgAttributeType=orgAttributeType;
            attrMap=attrMap;
        }

    let createUniform<'attr> 
            (poolId:Guid) 
            (ids:seq<OrgId>)
            (orgAttributeType:OrgAttributeType)
            (attrF: unit->'attr) =
        let attrMap = ids |> Seq.map(fun id->(id, attrF())) |> Map.ofSeq
        {
            MapOfOrgAttributes.id=poolId;
            orgAttributeType=orgAttributeType;
            attrMap=attrMap;
        }

    let makeMapOfAttributes<'attr> 
            (poolId:Guid) 
            (ids:seq<OrgId>)
            (orgAttributeType:OrgAttributeType)
            (locator:OrgId->'attr) = 
        create 
            poolId 
            ids
            orgAttributeType 
            (ids |> Seq.map locator)

