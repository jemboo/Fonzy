namespace global
open Microsoft.FSharpLu.Json
open System


type sorterShcSpec2Dto = 
    {
     rngGen:rngGenDto;
     sorter:sorterDto;
     mutSpec:sorterMutSpecDto;
     srtblStType:sortableSetTypeDto
     stWgtSpec:shcStageWeightSpecDto
     evalSpec:sorterEvalSpecDto
     annealer:annealerSpecDto;
     term:shcTermSpecDto; }

module SorterShcSpec2Dto =
    let fromDto (dto:sorterShcSpec2Dto) =
        result {
            let! rng = dto.rngGen |> RngGenDto.fromDto
            let! srt = dto.sorter |> SorterDto.fromDto
            let! mutSpec = dto.mutSpec |> SorterMutSpecDto.fromDto
            let! ssRs = dto.srtblStType |> SortableSetTypeDto.fromDto
            let! swS = dto.stWgtSpec |> ShcStageWeightSpecDto.fromDto
            let! evl = dto.evalSpec |> SorterEvalSpecDto.fromDto
            let! ann = dto.annealer  |> AnnealerSpecDto.fromDto
            let! term = dto.term  |> ShcTermSpecDto.fromDto
            return 
             {
                sorterShcSpec2.rngGen = rng;
                sorter = srt;
                mutatorSpec = mutSpec;
                srtblSetType = ssRs;
                sorterStageWeightSpec = swS;
                evalSpec = evl;
                annealerSpec = ann;
                termSpec = term;
             }
        }
                     
    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<sorterShcSpec2Dto> jstr
            return! fromDto dto
        }

    let toDto (sss:sorterShcSpec2) =
            {
                sorterShcSpec2Dto.rngGen = sss.rngGen |> RngGenDto.toDto;
                sorter = sss.sorter |> SorterDto.toDto;
                mutSpec = sss.mutatorSpec |> SorterMutSpecDto.toDto
                srtblStType = sss.srtblSetType |> SortableSetTypeDto.toDto
                stWgtSpec = sss.sorterStageWeightSpec |> ShcStageWeightSpecDto.toDto
                evalSpec = sss.evalSpec |> SorterEvalSpecDto.toDto
                annealer = sss.annealerSpec |> AnnealerSpecDto.toDto
                term = sss.termSpec  |> ShcTermSpecDto.toDto
            }

    let toJson (idt:sorterShcSpec2) =
        idt |> toDto |> Json.serialize



type sorterShcSpecRndGen2Dto = 
    {
         baseSpec:sorterShcSpec2Dto;
         sssrgT:sssrgTypeDto;
         rngGen: rngGenDto;
         count:int;
     }

module SorterShcSpecRndGen2Dto =
    let fromDto (dto:sorterShcSpecRndGen2Dto) =
        result {
            let! rng = dto.rngGen |> RngGenDto.fromDto
            let! srt = dto.sssrgT |> SssrgTypeDto.fromDto
            let! bs = dto.baseSpec |> SorterShcSpec2Dto.fromDto
            let! shcCt = dto.count |> ShcCount.create ""
            return 
             {
                sorterShcSpecRndGen2.rndGen = rng;
                baseSpec = bs;
                count = shcCt;
                sssrgType = srt;
             }
        }
                     
    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<sorterShcSpecRndGen2Dto> jstr
            return! fromDto dto
        }

    let toDto (sss:sorterShcSpecRndGen2) =
            {
                sorterShcSpecRndGen2Dto.rngGen = sss.rndGen |> RngGenDto.toDto;
                baseSpec = sss.baseSpec |> SorterShcSpec2Dto.toDto;
                count = sss.count |> ShcCount.value;
                sssrgT = sss.sssrgType |> SssrgTypeDto.toDto
            }

    let toJson (idt:sorterShcSpecRndGen2) =
        idt |> toDto |> Json.serialize


    let fromBase64 (jstr:string) =
        result {
            let! dto = jstr |> ByteUtils.base64ToObj |> Ok
            return dto
        }


    let toBase64 (idt:sorterShcSpecRndGen2) =
        idt |> ByteUtils.base64FromObj



type sorterShcResult2Dto = {
        shcId:string;
        msg:string; 
        archive:sorterShcArchDto
    }

module SorterShcResult2Dto =
    let fromDto (dto:sorterShcResult2Dto) =
        result {
            let! id =  dto.shcId |> Json.deserialize<Guid>
            let! arch = dto.archive |> SorterShcArchDto.fromDto
            return {
                        sorterShcResult2.id = id |> ShcId.fromGuid
                        msg = dto.msg;
                        archive = arch
                    }
        }

    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<sorterShcResult2Dto> jstr
            return! fromDto dto
        }

    let toDto (ssR:sorterShcResult2) =
        { 
          sorterShcResult2Dto.shcId =  ssR.id |> ShcId.value |> Json.serialize;
          msg=ssR.msg;
          archive = ssR.archive |> SorterShcArchDto.toDto
        }

    let toJson (ssR:sorterShcResult2) =
        ssR |> toDto |> Json.serialize




type sorterShcResults2Dto = {members:sorterShcResult2Dto[];}
module SorterShcResults2Dto =
    let fromDto (dto:sorterShcResults2Dto) =
        result {
            let! membs = dto.members |> Array.map(SorterShcResult2Dto.fromDto)
                        |> Array.toList
                        |> Result.sequence
                        
            return {
                sorterShcResults2.members = membs |> List.toArray;}
        }

    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<sorterShcResults2Dto> jstr
            return! fromDto dto
        }

    let toDto (ssR:sorterShcResults2) =
        { 
            sorterShcResults2Dto.members = 
                ssR.members |> Array.map(SorterShcResult2Dto.toDto)
        }

    let toJson (ssR:sorterShcResults2) =
        ssR |> toDto |> Json.serialize






type sorterShcResult25Dto = {
        shcId:string;
        generation:int; 
        generationSpan:int; 
        perfDto:sorterPerfDto;
    }