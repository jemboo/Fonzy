namespace global
open System

module AttributesDto =

    let fromDtoToInt (dto:string) = 
          Json.deserialize<int> dto

    let fromDtoToLatticeLoc2d (dto:string) = 
          Json.deserialize<LatticeLoc2d> dto

    let fromDtoToLatticeLoc3d (dto:string) = 
          Json.deserialize<LatticeLoc3d> dto

    let fromIntToDto (v:int) = 
          Json.serialize v

    let fromLatticeLoc2dToDto (loc2d:LatticeLoc2d) = 
          Json.serialize loc2d

    let fromLatticeLoc3dToDto (loc3d:LatticeLoc3d) = 
          Json.serialize loc3d


type MapOfOrgAttributesDto = 
    {
        id:Guid
        orgAttributeType:OrgAttributeType
        attributeMap:Map<Guid, string>
    }

module MapOfOrgAttributesDto =

    let fromDto<'attr> (attrConverter:string->Result<'attr, string>) (dto:MapOfOrgAttributesDto) =
        result {
            let orgAttributeType = dto.orgAttributeType
            let! mapTuples = dto.attributeMap 
                            |> Map.toSeq 
                            |> Seq.map(fun tup -> (tup |> fst |> OrgId.fromGuid, 
                                                   tup|> snd|> attrConverter))
                            |> Seq.map(fun a-> Result.map (fun ra -> ((fst a), ra)) (snd a))
                            |> Seq.toList
                            |> Result.sequence
            let mapRet = mapTuples |> Map.ofList

            return {
                MapOfOrgAttributes.id = dto.id
                orgAttributeType = orgAttributeType
                attrMap = mapRet
            }
        }


    let toDto<'attr> (strConverter: 'attr->string) (mapOfOrgAttributes:MapOfOrgAttributes<'attr>) =
        let attrM = mapOfOrgAttributes.attrMap 
                    |> Map.toSeq 
                    |> Seq.map(fun tup -> (tup |> fst |> OrgId.value, 
                                           tup |> snd |> strConverter))
                    |> Map.ofSeq
        {
            MapOfOrgAttributesDto.id = mapOfOrgAttributes.id
            orgAttributeType = mapOfOrgAttributes.orgAttributeType
            attributeMap = attrM
        }


