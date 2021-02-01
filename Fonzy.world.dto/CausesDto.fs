namespace global
open System



type KeyedCauseDto = {id:Guid; map:Map<string, string>}
module KeyedCauseDto =
                      
    let toDto (tup:Guid * Map<string, string>) =
        {KeyedCauseDto.id= fst tup;
        KeyedCauseDto.map= snd tup;}

    let fromDto (eDto:KeyedCauseDto) =
            result {
                let id = eDto.id
                let map = eDto.map
                return KeyedCauses.ofKeyed eDto.id eDto.map
            }


type CauseTypeDto = {cat:string; value:string}
module CauseTypeDto =

    let toDto (ct:CauseType) =
        match ct with
        | CauseType.Destroy -> {cat="Destroy"; value = Json.serialize None}
        | CauseType.NoOp -> {cat="NoOp"; value = Json.serialize None}
        | CauseType.Keyed (id, map) ->  {cat="Keyed"; value = Json.serialize(id, map)}

    let fromDto (eDto:CauseTypeDto) =
        if eDto.cat = "Destroy" then
            result {
                return CauseType.Destroy
            }
        else if eDto.cat = "NoOp" then
            result {
                return CauseType.NoOp
            }
        else if eDto.cat = "Keyed" then
            result {
                let! tup = Json.deserialize<Tuple<Guid,Map<string,string>>> eDto.value
                return CauseType.Keyed tup
            }
        else sprintf "cat: %s for CauseTypeDto not found"
                      eDto.cat |> Error


// no serialization of CauseDto