namespace global

type SorterPerfBinsDto = {switchCount:int; stageCount:int; binCount:int}
module SorterPerfBinsDto =
    let toDto (tup:SortingEval.SorterPerfBin*int) =
        let perfBin = fst tup
        {
            SorterPerfBinsDto.switchCount = (SwitchCount.value perfBin.usedSwitchCount)
            SorterPerfBinsDto.stageCount = (StageCount.value perfBin.usedStageCount)
            SorterPerfBinsDto.binCount = snd tup
        }

    let toTup (dto:SorterPerfBinsDto) =
        result {
            let! uwc = SwitchCount.create "" dto.switchCount
            let! utc = StageCount.create "" dto.stageCount
            return (  
                      { 
                        SortingEval.SorterPerfBin.usedSwitchCount = uwc
                        SortingEval.SorterPerfBin.usedStageCount = utc
                      },
                      dto.binCount
                   )
         }

    let fromDtos (dtos:SorterPerfBinsDto[]) =
        result {
                let! tups = dtos |> Array.map(toTup)
                                |> Array.toList
                                |> Result.sequence

                return tups |> List.toArray
            }

    let toDtos (perfBins:(SortingEval.SorterPerfBin*int)[]) =
        perfBins |> Array.map(toDto)

    let toJson (perfBins:(SortingEval.SorterPerfBin*int)[]) =
        perfBins |> toDtos |> Json.serialize

    let fromJson (json:string) =
        result {
            let! dto = Json.deserialize<SorterPerfBinsDto[]> json
            return! dto |> fromDtos
        }
