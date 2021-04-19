namespace global
open System

type WorldDto = {id:Guid; parentId:Guid; causeSpecDto:CauseSpecDto; 
                 enviroDto:EnviroDto}
module WorldDto = 
    let toDto (w:World) =
        {WorldDto.id = WorldId.value w.id;
         WorldDto.parentId = WorldId.value w.parentId;
         WorldDto.causeSpecDto = w.cause.causeSpec |> CauseSpecDto.toDto;
         WorldDto.enviroDto = w.enviro |> EnviroDto.toDto}

    let toJson (w:World) =
        w |> toDto |> Json.serialize

    let fromDto (wDto:WorldDto) =
           result {
             let worldId = WorldId.fromGuid wDto.id
             let parentId = WorldId.fromGuid wDto.parentId
             let! e = wDto.enviroDto |> EnviroDto.fromDto
             let! cs = wDto.causeSpecDto |> CauseSpecDto.fromDto;
             let! c = cs |> Causes.fromCauseSpec
             return  {
                 World.id = worldId;
                 World.parentId = parentId
                 World.cause = c
                 World.enviro = e
                } 
            }

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<WorldDto> js
            return! fromDto dto
        }
            
type WorldActionDto = {childId:Guid; parentWorldDto:WorldDto; 
                       causeSpecDto:CauseSpecDto;}
module WorldActionDto = 
    let toDto (w:WorldAction) =
        {
          WorldActionDto.childId = WorldId.value w.childId;
          WorldActionDto.parentWorldDto = w.parentWorld |> WorldDto.toDto;
          WorldActionDto.causeSpecDto = w.cause.causeSpec |> CauseSpecDto.toDto;
        }

    let toJson (w:WorldAction) =
        w |> toDto |> Json.serialize

    let fromDto (waDto:WorldActionDto) =
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
            let! dto = Json.deserialize<WorldActionDto> js
            return! fromDto dto
        }

type WorldMergeDto = {id:Guid; sourceNameMap: Map<string,Guid>; 
                      mergeMapItems:MergeMapItem[]; enviroDto:EnviroDto}
module WorldMergeDto = 
    let toDto (w:WorldMerge) =
        {
          WorldMergeDto.id = WorldMergeId.value w.id;
          WorldMergeDto.sourceNameMap = w.sourceNameMap;
          WorldMergeDto.mergeMapItems = w.mergeMapItems |> List.toArray
          WorldMergeDto.enviroDto = w.enviro |> EnviroDto.toDto
        }

    let toJson (w:WorldMerge) =
        w |> toDto |> Json.serialize

    let fromDto (waDto:WorldMergeDto) =
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
            let! dto = Json.deserialize<WorldMergeDto> js
            return! fromDto dto
        }