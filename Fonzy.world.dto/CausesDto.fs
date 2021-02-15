namespace global
open System


type CauseSpecDto = {id:Guid; genus:string[]; prams:Map<string,string>; keyMap:Map<string,string>}
module CauseSpecDto =
    let toDto (cs:CauseSpec) =
        {CauseSpecDto.id = cs.id;
         CauseSpecDto.genus = cs.genus|> List.toArray;
         CauseSpecDto.prams = cs.prams;
         CauseSpecDto.keyMap = cs.keyMap}

    let toJson (cs:CauseSpec) =
        cs |> toDto |> Json.serialize

    let fromDto (csDto:CauseSpecDto) =
            {CauseSpec.id = csDto.id;
             CauseSpec.genus = csDto.genus |> Array.toList;
             CauseSpec.prams = csDto.prams;
             CauseSpec.keyMap = csDto.keyMap
            } |> Ok

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<CauseSpecDto> js
            return! fromDto dto
        }


// no serialization of CauseDto