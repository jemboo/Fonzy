namespace global
open System

type causeSpecDto = {id:Guid; genus:string[]; prams:stringMapDto;}
module CauseSpecDto =
    let toDto (cs:causeSpec) =
        {causeSpecDto.id = CauseSpecId.value cs.id;
         causeSpecDto.genus = cs.genus|> List.toArray;
         causeSpecDto.prams = cs.prams;}

    let toJson (cs:causeSpec) =
        cs |> toDto |> Json.serialize

    let fromDto (csDto:causeSpecDto) =
            {causeSpec.id = CauseSpecId.fromGuid csDto.id;
             genus = csDto.genus |> Array.toList;
             prams = csDto.prams;
            } |> Ok

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<causeSpecDto> js
            return! fromDto dto
        }


// no serialization of CauseDto