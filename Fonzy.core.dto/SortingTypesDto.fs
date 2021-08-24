namespace global

type switchOrStageCountDto = {wOrT:string; value:int;}
module SwitchOrStageCountDto =
    
    let toDto (sorterLength:SwitchOrStageCount) =
        match sorterLength with
        | SwitchOrStageCount.Switch ct -> {wOrT="Switch"; value=(SwitchCount.value ct)}
        | SwitchOrStageCount.Stage ct -> {wOrT="Stage"; value=(StageCount.value ct);}

    let toJson (sorterLength:SwitchOrStageCount) =
        sorterLength |> toDto |> Json.serialize

    let fromDto (dto:switchOrStageCountDto) =
        let parseCat cat count =
            match cat with
            | "Switch" -> SwitchOrStageCount.Switch 
                                    ((SwitchCount.create "" count)|> Result.ExtractOrThrow) |> Ok
            | "Stage" -> SwitchOrStageCount.Stage 
                                    ((StageCount.create "" count)|> Result.ExtractOrThrow) |> Ok
            | _ -> Error (sprintf "no match for SwitchOrStageCountDto: %s" cat)
        result {
            return! parseCat dto.wOrT dto.value
        }

    let fromJson (cereal:string) =
        result {
            let! dto = cereal |> Json.deserialize<switchOrStageCountDto>
            return! dto |> fromDto
        }


type switchUsesDto = {switchCount:int; weights:int[]}
module SwitchUsesDto =
    let fromDto (dto:switchUsesDto) =
        result {
            return SwitchUses.init dto.weights
        }

    let fromJson (cereal:string) =
        result {
            let! sorterDto = cereal |> Json.deserialize<switchUsesDto>
            return! fromDto sorterDto
        }

    let toDto (switchUses:switchUses) =
        {switchUsesDto.switchCount= (SwitchUses.switchCount switchUses); 
         weights = (SwitchUses.getWeights switchUses)}

    let toJson (switchUses:switchUses) =
        switchUses |> toDto |> Json.serialize
