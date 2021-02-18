namespace global
open System


type EnviroDto = {cat:string; value:string}
module EnviroDto =
    let toDto (env:Enviro) =
        match env with
        | Enviro.ObjectMap objectMap -> {
                        cat="ObjectMap"; 
                        value = Json.serialize objectMap}
        | Enviro.Empty -> {cat = "Empty"; value = Json.serialize None}
        
        | MergeSet s -> {cat = "MergeSet"; value = s |> Json.serialize}

    let toJson (idt:Enviro) =
        idt |> toDto |> Json.serialize

    let fromDto (eDto:EnviroDto) =
        if eDto.cat = "Empty" then
            result {
                return Enviro.Empty
            }
        else if eDto.cat = "ObjectMap" then
            result {
                let! b = Json.deserialize<Map<string, string>> eDto.value
                return Enviro.ObjectMap b
            }
        else if eDto.cat = "MergeSet" then
            result {
                let! b = Json.deserialize<Map<Guid, Map<string, string>>> eDto.value
                return Enviro.MergeSet b
            }
        else sprintf "cat: %s for EnviroDto not found"
                      eDto.cat |> Error

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<EnviroDto> js
            return! fromDto dto
        }