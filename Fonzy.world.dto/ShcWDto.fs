namespace global

type sorterShcResultDto = {sorterShc:sorterShcSpecDto; cat:string; rept:string}
module SorterShcResultDto =
    let fromDto (dto:sorterShcResultDto) =
        result {
            let! spec = dto.sorterShc |> SorterShcSpecDto.fromDto
            return {sorterShcResult.spec = spec;
                    sorterShcResult.cat = dto.cat;
                    sorterShcResult.report = dto.rept}
        }

    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<sorterShcResultDto> jstr
            return! fromDto dto
        }

    let toDto (ssR:sorterShcResult) =
        { sorterShcResultDto.sorterShc = ssR.spec |> SorterShcSpecDto.toDto;
          cat=ssR.cat;
          rept = ssR.report}

    let toJson (ssR:sorterShcResult) =
        ssR |> toDto |> Json.serialize



type sorterShcResultsDto = {members:sorterShcResultDto[];}
module SorterShcResultsDto =
    let fromDto (dto:sorterShcResultsDto) =
        result {
            let! membs = dto.members |> Array.map(SorterShcResultDto.fromDto)
                        |> Array.toList
                        |> Result.sequence
                        
            return {
                sorterShcResults.members = membs |> List.toArray;}
        }

    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<sorterShcResultsDto> jstr
            return! fromDto dto
        }

    let toDto (ssR:sorterShcResults) =
        { 
            sorterShcResultsDto.members = 
                ssR.members |> Array.map(SorterShcResultDto.toDto)
        }

    let toJson (ssR:sorterShcResults) =
        ssR |> toDto |> Json.serialize