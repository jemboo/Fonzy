namespace global
open System

type worldDto = {id:Guid; parentId:Guid; causeSpecDto:causeSpecDto; 
                 enviroDto:enviroDto}
module WorldDto = 
    let toDto (w:world) =
        {worldDto.id = WorldId.value w.id;
         worldDto.parentId = WorldId.value w.parentId;
         worldDto.causeSpecDto = w.cause.causeSpec |> CauseSpecDto.toDto;
         worldDto.enviroDto = w.enviro |> EnviroDto.toDto}

    let toJson (w:world) =
        w |> toDto |> Json.serialize

    let fromDto (wDto:worldDto) =
           result {
             let worldId = WorldId.fromGuid wDto.id
             let parentId = WorldId.fromGuid wDto.parentId
             let! e = wDto.enviroDto |> EnviroDto.fromDto
             let! cs = wDto.causeSpecDto |> CauseSpecDto.fromDto;
             let! c = cs |> Causes.fromCauseSpec
             return  {
                 world.id = worldId;
                 parentId = parentId
                 cause = c
                 enviro = e
                } 
            }

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<worldDto> js
            return! fromDto dto
        }
            
type worldActionDto = {childId:Guid; parentWorldDto:worldDto; 
                       causeSpecDto:causeSpecDto;}
module WorldActionDto = 
    let toDto (w:WorldAction) =
        {
          worldActionDto.childId = WorldId.value w.childId;
          worldActionDto.parentWorldDto = w.parentWorld |> WorldDto.toDto;
          worldActionDto.causeSpecDto = w.cause.causeSpec |> CauseSpecDto.toDto;
        }

    let toJson (w:WorldAction) =
        w |> toDto |> Json.serialize

    let fromDto (waDto:worldActionDto) =
           result {
             let! pw = waDto.parentWorldDto |> WorldDto.fromDto
             let! cs = waDto.causeSpecDto |> CauseSpecDto.fromDto;
             let! c = cs |> Causes.fromCauseSpec
             return  {
                 WorldAction.childId = WorldId.fromGuid waDto.childId;
                 WorldAction.parentWorld = pw
                 WorldAction.cause = c
                } 
            }

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<worldActionDto> js
            return! dto |> fromDto
        }

type worldMergeDto = {id:Guid; sourceNameMap: Map<string,Guid>; 
                      mergeMapItems:MergeMapItem[]; enviroDto:enviroDto}
module WorldMergeDto = 
    let toDto (w:WorldMerge) =
        {
          worldMergeDto.id = WorldMergeId.value w.id;
          worldMergeDto.sourceNameMap = w.sourceNameMap;
          worldMergeDto.mergeMapItems = w.mergeMapItems |> List.toArray
          worldMergeDto.enviroDto = w.enviro |> EnviroDto.toDto
        }

    let toJson (w:WorldMerge) =
        w |> toDto |> Json.serialize

    let fromDto (waDto:worldMergeDto) =
           result {
             let mergeMapItems = waDto.mergeMapItems |> Array.toList
             let! worldMergeId = WorldMergeId.create waDto.id
             let! enviro = waDto.enviroDto |> EnviroDto.fromDto
             return  {
                         WorldMerge.id = worldMergeId;
                         WorldMerge.sourceNameMap = waDto.sourceNameMap;
                         WorldMerge.mergeMapItems = mergeMapItems;
                         WorldMerge.enviro = enviro
                } 
            }

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<worldMergeDto> js
            return! fromDto dto
        }