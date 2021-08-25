namespace global
open System

type enviroDto = {cat:string; value:string}
module EnviroDto =
    let toDto (env:enviro) =
        match env with
        | enviro.ObjectMap objectMap -> {
                        cat= nameof enviro.ObjectMap; 
                        value = Json.serialize objectMap}
        | enviro.Empty -> {
                        cat = nameof enviro.Empty; 
                        value = Json.serialize None}

    let toJson (idt:enviro) =
        idt |> toDto |> Json.serialize

    let fromDto (eDto:enviroDto) =
        match eDto.cat with
        | nameof enviro.ObjectMap -> result {
            let! b = Json.deserialize<Map<string, string>> eDto.value
            return enviro.ObjectMap b
            }
        | nameof enviro.Empty -> enviro.Empty |> Ok
        | t -> (sprintf "Invalid Enviro Type: %s" t) |> Error

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<enviroDto> js
            return! fromDto dto
        }