namespace global

type sorterPerfBinDto = int[]
module SorterPerfBinDto =
    let toDto (spb:SortingEval.sorterPerfBin) =
        [|
            (SwitchCount.value spb.usedSwitchCount)
            (StageCount.value spb.usedStageCount)
            (SorterCount.value spb.sorterCount)
            spb.successCount
            spb.failCount
        |]

    let toTup (dto:sorterPerfBinDto) =
        result {
            let! uwc = SwitchCount.create "" dto.[0]
            let! utc = StageCount.create "" dto.[1]
            let! stc = SorterCount.create "" dto.[2]
            return
                { 
                    SortingEval.sorterPerfBin.usedSwitchCount = uwc;
                    SortingEval.sorterPerfBin.usedStageCount = utc;
                    SortingEval.sorterPerfBin.sorterCount = stc;
                    SortingEval.sorterPerfBin.successCount = dto.[3]
                    SortingEval.sorterPerfBin.failCount = dto.[4]
                }
        }

    let fromDtos (dtos:sorterPerfBinDto[]) =
        result {
                let! tups = dtos |> Array.map(toTup)
                                 |> Array.toList
                                 |> Result.sequence

                return tups |> List.toArray
            }

    let toDtos (perfBins:SortingEval.sorterPerfBin[]) =
        perfBins |> Array.map(toDto)

    let toJson (perfBins:SortingEval.sorterPerfBin[]) =
        perfBins |> toDtos |> Json.serialize

    let fromJson (json:string) =
        result {
            let! dto = Json.deserialize<sorterPerfBinDto[]> json
            return! dto |> fromDtos
        }