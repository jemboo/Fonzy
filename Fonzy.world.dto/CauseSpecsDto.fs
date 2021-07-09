namespace global
open System

type causeSpecDto = {id:Guid; genus:string[]; prams:Map<string,string>;}
module CauseSpecDto =
    let toDto (cs:CauseSpec) =
        {causeSpecDto.id = CauseSpecId.value cs.id;
         causeSpecDto.genus = cs.genus|> List.toArray;
         causeSpecDto.prams = cs.prams;}

    let toJson (cs:CauseSpec) =
        cs |> toDto |> Json.serialize

    let fromDto (csDto:causeSpecDto) =
            {CauseSpec.id = CauseSpecId.fromGuid csDto.id;
             CauseSpec.genus = csDto.genus |> Array.toList;
             CauseSpec.prams = csDto.prams;
            } |> Ok

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<causeSpecDto> js
            return! fromDto dto
        }


// no serialization of CauseDto