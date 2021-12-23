namespace global
open System


type sorterSetPerfDto = { sorterSetId:Guid; 
                          sorterRndGenDto:sorterRndGenDto;
                          rngGenDto:rngGenDto;
                          sorterCount:int
                          srtblStTypDto:sortableSetTypeDto
                          perfBinsDto:sorterPerfBinDto[];
                        }


module SorterSetPerfDto =

    let fromDto (dto:sorterSetPerfDto) =

        result {
            let! ssid = SorterSetId.create dto.sorterSetId
            let! srtrRndGn = dto.sorterRndGenDto |> SorterRndGenDto.fromDto
            let! rngn = dto.rngGenDto |> RngGenDto.fromDto
            let! srtrCt = dto.sorterCount |> SorterCount.create ""
            let! srtblStType = dto.srtblStTypDto|> SortableSetTypeDto.fromDto
            let! perfBins = dto.perfBinsDto |> SorterPerfBinDto.fromDtos
            return {
                      sorterSetPerf.id = ssid;
                      sorterSetPerf.sorterRndGen = srtrRndGn;
                      sorterSetPerf.rngGen = rngn;
                      sorterSetPerf.sorterCount = srtrCt;
                      sorterSetPerf.sortableSetType = srtblStType
                      sorterSetPerf.perfBins = perfBins
                   }
        }


    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<sorterSetPerfDto> jstr
            return! fromDto dto
        }

    let toDto (ssg:sorterSetPerf) =
        {
            sorterSetPerfDto.sorterSetId = (SorterSetId.value ssg.id)
            sorterSetPerfDto.sorterRndGenDto = ssg.sorterRndGen |> SorterRndGenDto.toDto
            sorterSetPerfDto.rngGenDto = ssg.rngGen |> RngGenDto.toDto
            sorterSetPerfDto.sorterCount = (SorterCount.value ssg.sorterCount)
            sorterSetPerfDto.srtblStTypDto = ssg.sortableSetType |> SortableSetTypeDto.toDto
            sorterSetPerfDto.perfBinsDto = ssg.perfBins |> Array.map(SorterPerfBinDto.toDto)
        }

    let toJson (idt:sorterSetPerf) =
        idt |> toDto |> Json.serialize



type sorterSetMergedPerfDto = 
    {
        mergeCt:int;
        srtblStTypDto:sortableSetTypeDto
        sorterRndGenDto:sorterRndGenDto;
        perfBinsDto:sorterPerfBinDto[];
    }


module SorterSetMergedPerfDto =

    let merge (a:sorterSetPerfDto seq) =
        let allin = a |> Seq.toArray

        let _mergeSorterPerfBins (dtos:sorterSetPerfDto seq) =
            result {
                let! lols = dtos |> Seq.map(fun dto -> dto.perfBinsDto |> SorterPerfBinDto.fromIntArrays)
                                 |> Seq.toList
                                 |> Result.sequence

                let mergedBins = lols |> Seq.concat
                                      |> SortingEval.SorterPerfBin.merge
                return mergedBins |> Seq.map(SorterPerfBinDto.toDto)
                                  |> Seq.toArray
            }

        result {

            let! prfBinsDto =  _mergeSorterPerfBins allin

            return {
                sorterSetMergedPerfDto.mergeCt = allin.Length;
                srtblStTypDto = allin.[0].srtblStTypDto;
                sorterRndGenDto = allin.[0].sorterRndGenDto;
                perfBinsDto = prfBinsDto;
            }
         }

    let pivotTableHdrs = sprintf "mergeCt\t %s\t success\t binCt\t usedSwitches\t usedStages" SorterRndGen.reportHeaders

    let toReport (mDto:sorterSetMergedPerfDto) = 
        result {
        
           let! srterRndGen = mDto.sorterRndGenDto |> SorterRndGenDto.fromDto
           let srterRndGenInfo = srterRndGen |> SorterRndGen.reportString
           let mergeCt = mDto.mergeCt |> string

           let repPfx = sprintf "%s\t %s" 
                            mergeCt srterRndGenInfo

           let repLine (spb: SortingEval.sorterPerfBin) = 
               let goodOnes = sprintf "%s\t %s\t %s\t %s\t %s\t" 
                               repPfx 
                               "true"
                               (spb.successCount |> string) 
                               (spb.usedSwitchCount |> SwitchCount.value |> string) 
                               (spb.usedStageCount|> StageCount.value |> string)
               let badOnes = sprintf "%s\t %s\t %s\t %s\t %s\t" 
                                repPfx 
                                "false"
                                (spb.failCount |> string) 
                                (spb.usedSwitchCount |> SwitchCount.value |> string) 
                                (spb.usedStageCount|> StageCount.value |> string)
               sprintf "%s\n%s" goodOnes badOnes

           let! perfPins = mDto.perfBinsDto |> SorterPerfBinDto.fromDtos

           return perfPins |> Array.map (repLine)
        }