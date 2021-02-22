namespace global

type SorterMutationTypeDto = {mType:string; rate:float}
module SorterMutationTypeDto =
    let fromDto (dto:SorterMutationTypeDto) =
        result {
                let! mr = MutationRate.create "" dto.rate
                match dto.mType with
                | "Switch" -> return SorterMutationType.Switch mr
                | "Stage" ->  return SorterMutationType.Stage mr
                | _ ->        let! res =  Error (sprintf "no match for SorterMutationType: %s" dto.mType)
                              return res
            }

    let toDto (mutationType:SorterMutationType) =
        match mutationType with
        | SorterMutationType.Switch mr -> {mType="Switch"; rate=MutationRate.value mr}
        | SorterMutationType.Stage mr -> {mType="Stage"; rate=MutationRate.value mr}

    let toJson (mutationType:SorterMutationType) =
        toDto mutationType |> Json.serialize

    let fromJson (json:string) =
        result {
            let! dto = Json.deserialize<SorterMutationTypeDto> json
            return! dto |> fromDto
        }
