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

    let toJson (idt:Enviro) =
        idt |> toDto |> Json.serialize

    let fromDto (eDto:enviroDto) =
        match eDto.cat with
        | nameof Enviro.ObjectMap -> result {
            let! b = Json.deserialize<Map<string, string>> eDto.value
            return Enviro.ObjectMap b
            }
        | nameof Enviro.Empty -> Enviro.Empty |> Ok
        | t -> (sprintf "Invalid Enviro Type: %s" t) |> Error

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<enviroDto> js
            return! fromDto dto
        }