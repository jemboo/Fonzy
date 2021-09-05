namespace global

type annealerSpecDto = {cat:string; value:string;}
module AnnealerSpecDto =
    let fromDto (dto:annealerSpecDto) =
        match dto.cat with
        | nameof annealerSpec.Constant ->
                result {
                    let! b = Json.deserialize<float> dto.value
                    let! tmp = b |> Temp.create ""
                    return annealerSpec.Constant tmp
                }
        | nameof annealerSpec.Exp ->
                result {
                    let! b = Json.deserialize<float[]> dto.value
                    let! tmp = b.[0] |> Temp.create ""
                    return annealerSpec.Exp (tmp, b.[1]) 
                }
        | t -> sprintf "cat: %s for shcStageWeightSpecDto not found"
                     dto.cat |> Error

    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<annealerSpecDto> jstr
            return! fromDto dto
        }

    let toDto (ssD:annealerSpec) =
        match ssD with
        | annealerSpec.Constant tmp -> 
                { annealerSpecDto.cat = nameof annealerSpec.Constant; 
                    value = (Temp.value tmp) |> Json.serialize}
        | annealerSpec.Exp (tmp, dec) -> 
                { annealerSpecDto.cat = nameof annealerSpec.Exp; 
                    value = [|(Temp.value tmp); dec|]
                            |> Json.serialize}


    let toJson (idt:annealerSpec) =
        idt |> toDto |> Json.serialize

