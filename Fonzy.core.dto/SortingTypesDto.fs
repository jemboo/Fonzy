namespace global

type SorterLengthDto = {wOrT:string; value:int;}
module SorterLengthDto =
    
    let toDto (sorterLength:SorterLength) =
        match sorterLength with
        | SorterLength.Switch ct -> {wOrT="Switch"; value=(SwitchCount.value ct)}
        | SorterLength.Stage ct -> {wOrT="Stage"; value=(StageCount.value ct);}

    let toJson (sorterLength:SorterLength) =
        sorterLength |> toDto |> Json.serialize

    let fromDto (dto:SorterLengthDto) =
        let parseCat cat count =
            match cat with
            | "Switch" -> SorterLength.Switch 
                                    ((SwitchCount.create "" count)|> Result.ExtractOrThrow) |> Ok
            | "Stage" -> SorterLength.Stage 
                                    ((StageCount.create "" count)|> Result.ExtractOrThrow) |> Ok
            | _ -> Error (sprintf "no match for SorterLengthDto: %s" cat)
        result {
            return! parseCat dto.wOrT dto.value
        }

    let fromJson (cereal:string) =
        result {
            let! dto = cereal |> Json.deserialize<SorterLengthDto>
            return! dto |> fromDto
        }