namespace global
open System



type WorldDto = {id:Guid; parentId:Guid; causeSpecDto:CauseSpecDto; 
                 enviroDto:EnviroDto}
module WorldDto = 
    let toDto (w:World) =
        {WorldDto.id = w.id;
         WorldDto.parentId = w.parentId;
         WorldDto.causeSpecDto = w.cause.causeSpec |> CauseSpecDto.toDto;
         WorldDto.enviroDto = w.enviroment |> EnviroDto.toDto}

    let toJson (w:World) =
        w |> toDto |> Json.serialize

    let fromDto (wDto:WorldDto) =
           result {
             let! e = wDto.enviroDto |> EnviroDto.fromDto
             let! cs = wDto.causeSpecDto |> CauseSpecDto.fromDto;
             let! c = cs |> Causes.fromCauseSpec
             return  {
                 World.id = wDto.id;
                 World.parentId = wDto.parentId
                 World.cause = c
                 World.enviroment = e
                } 
            }

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<WorldDto> js
            return! fromDto dto
        }
            
            
            
type WorldActionDto = {childId:Guid; parentWorldDto:WorldDto; causeSpecDto:CauseSpecDto;}
module WorldActionDto = 
    let toDto (w:WorldAction) =
        {
          WorldActionDto.childId = w.childId;
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
                 WorldAction.childId = waDto.childId;
                 WorldAction.parentWorld = pw
                 WorldAction.cause = c
                } 
            }

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<WorldActionDto> js
            return! fromDto dto
        }
