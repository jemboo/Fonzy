namespace global
open System


type enviroDto = {cat:string; value:string}
module EnviroDto =
    let toDto (env:Enviro) =
        match env with
        | Enviro.ObjectMap objectMap -> {
                        cat="ObjectMap"; 
                        value = Json.serialize objectMap}
        | Enviro.Empty -> {cat = "Empty"; value = Json.serialize None}
        | Enviro.RootDto dto -> {cat = "RootDto"; value = Json.serialize dto}

    let toJson (idt:Enviro) =
        idt |> toDto |> Json.serialize

    let fromDto (eDto:enviroDto) =
        if eDto.cat = "Empty" then
            result {
                return Enviro.Empty
            }
        else if eDto.cat = "ObjectMap" then
            result {
                let! b = Json.deserialize<Map<string, string>> eDto.value
                return Enviro.ObjectMap b
            }
        else if eDto.cat = "RootDto" then
            result {
                let! b = Json.deserialize<RootDto> eDto.value
                return Enviro.RootDto b
            }

        else sprintf "cat: %s for EnviroDto not found"
                      eDto.cat |> Error

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<enviroDto> js
            return! fromDto dto
        }