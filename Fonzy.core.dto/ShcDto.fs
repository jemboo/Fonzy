﻿namespace global
open Microsoft.FSharpLu.Json

type shcStageWeightSpecDto = {cat:string; value:string}
module ShcStageWeightSpecDto =
    let fromDto (dto:shcStageWeightSpecDto) =
        match dto.cat with
        | nameof sorterStageWeightSpec.Constant ->
                result {
                    let! b = Json.deserialize<float> dto.value
                    let! sw = b |> StageWeight.create
                    return sorterStageWeightSpec.Constant sw
                }
        | t -> sprintf "cat: %s for shcStageWeightSpecDto not found"
                     dto.cat |> Error

    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<shcStageWeightSpecDto> jstr
            return! fromDto dto
        }

    let toDto (ssD:sorterStageWeightSpec) =
        match ssD with
        | sorterStageWeightSpec.Constant sw -> 
                { shcStageWeightSpecDto.cat = nameof sorterStageWeightSpec.Constant; 
                    value = (StageWeight.value sw) |> Json.serialize}

    let toJson (idt:sorterStageWeightSpec) =
        idt |> toDto |> Json.serialize


type sorterMutSpecDto = {cat:string; value:string}
module SorterMutSpecDto =
    let fromDto (dto:sorterMutSpecDto) =
        match dto.cat with
        | nameof sorterMutSpec.Constant ->
                result {
                    let! smt = dto.value |> SorterMutTypeDto.fromJson
                    return sorterMutSpec.Constant smt
                }
        | t -> sprintf "cat: %s for sorterMutSpecDto not found"
                     dto.cat |> Error

    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<sorterMutSpecDto> jstr
            return! fromDto dto
        }

    let toDto (ssD:sorterMutSpec) =
        match ssD with
        | sorterMutSpec.Constant smt -> 
                { sorterMutSpecDto.cat = nameof sorterMutSpec.Constant; 
                  value = smt |> SorterMutTypeDto.toJson }

    let toJson (idt:sorterMutSpec) =
        idt |> toDto |> Json.serialize

        
type sorterEvalSpecDto = {cat:string; value:string}
module SorterEvalSpecDto =
    let fromDto (dto:sorterEvalSpecDto) =
        match dto.cat with
        | nameof sorterEvalSpec.PerfBin -> sorterEvalSpec.PerfBin |> Ok
        | t -> sprintf "cat: %s for sorterEvalSpecDto not found"
                        dto.cat |> Error
        
    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<sorterEvalSpecDto> jstr
            return! fromDto dto
        }
        
    let toDto (ssD:sorterEvalSpec) =
        match ssD with
        | sorterEvalSpec.PerfBin -> 
                { sorterEvalSpecDto.cat = nameof sorterEvalSpec.PerfBin; 
                    value = ""}
        
    let toJson (idt:sorterEvalSpec) =
        idt |> toDto |> Json.serialize


type shcSaveDetailsDto = {cat:string; value:string}
module ShcSaveDetailsDto =
    let fromDto (dto:shcSaveDetailsDto) =
        match dto.cat with
        | nameof shcSaveDetails.Always -> shcSaveDetails.Always |> Ok
        | nameof shcSaveDetails.IfBest -> shcSaveDetails.IfBest |> Ok
        | nameof shcSaveDetails.BetterThanLast -> shcSaveDetails.BetterThanLast |> Ok
        | nameof shcSaveDetails.ForSteps -> 
            result {
                let! sav = dto.value |> Json.deserialize<int[]>
                let sl = sav |> Array.map(StepNumber.fromInt)
                return shcSaveDetails.ForSteps sl
            }
        | nameof shcSaveDetails.EnergyThresh ->
                result {
                    let! b = dto.value |> Json.deserialize<float>
                    let! e = Energy.create "" b
                    return shcSaveDetails.EnergyThresh e
                }
        | t -> sprintf "cat: %s for shcSaveDetailsDto not found"
                     dto.cat |> Error

    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<shcSaveDetailsDto> jstr
            return! fromDto dto
        }

    let toDto (ssD:shcSaveDetails) =
        match ssD with
        | shcSaveDetails.Always -> 
                { shcSaveDetailsDto.cat = nameof shcSaveDetails.Always; 
                    value=""}
        | shcSaveDetails.IfBest -> 
                { shcSaveDetailsDto.cat = nameof shcSaveDetails.IfBest; 
                    value=""}
        | shcSaveDetails.BetterThanLast -> 
                { shcSaveDetailsDto.cat = nameof shcSaveDetails.BetterThanLast; 
                  value=""}
        | shcSaveDetails.EnergyThresh e -> 
                { shcSaveDetailsDto.cat = nameof shcSaveDetails.EnergyThresh; 
                  value = (Energy.value e) |> Json.serialize }
        | shcSaveDetails.ForSteps stps -> 
                { shcSaveDetailsDto.cat = nameof shcSaveDetails.ForSteps; 
                  value = stps |> Array.map(StepNumber.value) 
                               |> Json.serialize}

    let toJson (idt:shcSaveDetails) =
        idt |> toDto |> Json.serialize


type shcTermSpecDto = {cat:string; value:string}
module ShcTermSpecDto =
    let fromDto (dto:shcTermSpecDto) =
        match dto.cat with
        | nameof shcTermSpec.FixedLength ->
            result {
                let! b = dto.value |> Json.deserialize<int>
                let! sn = StepNumber.create "" b
                return shcTermSpec.FixedLength sn
            }
        | nameof shcTermSpec.EnergyBased ->
                result {
                    let! aa = dto.value |> Json.deserialize<string[]>
                    let! ev = aa.[0] |> Json.deserialize<float>
                    let! nv = aa.[1] |> Json.deserialize<int>
                    let! xv = aa.[2] |> Json.deserialize<int>
                    let! e = Energy.create "" ev
                    let! ns = StepNumber.create "" nv
                    let! xs = StepNumber.create "" xv
                    return shcTermSpec.EnergyBased (e, ns, xs)
                }
        | t -> sprintf "cat: %s for IntDistTypeDto not found"
                     dto.cat |> Error

                     
    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<shcTermSpecDto> jstr
            return! fromDto dto
        }

    let toDto (ssD:shcTermSpec) =
        match ssD with
        | shcTermSpec.FixedLength st -> 
            { shcTermSpecDto.cat = nameof shcTermSpec.FixedLength; 
                value = st |> StepNumber.value |> Json.serialize }
        | shcTermSpec.EnergyBased (e, ns, xs) ->
            let ev = e |> Energy.value |> Json.serialize 
            let mst = ns |> StepNumber.value |> Json.serialize 
            let xst = xs |> StepNumber.value |> Json.serialize 
            let vv = [|ev; mst; xst|] |> Json.serialize 
            { shcTermSpecDto.cat = nameof shcTermSpec.EnergyBased; 
              value = vv }

    let toJson (idt:shcTermSpec) =
        idt |> toDto |> Json.serialize


type sorterShcArchDto = 
    {step:int;
     rev:int; 
     rngGen:rngGenDto option;
     sorter:sorterDto option;
     switchUses:switchUsesDto option;
     perf:sorterPerfDto;
     energy:float; }

module SorterShcArchDto =
    let fromDto (dto:sorterShcArchDto) =
        result {
            let! st = dto.step |> StepNumber.create "";
            let! rev = dto.rev |> RevNumber.create "";
            let! rng = dto.rngGen |> Result.bindOption RngGenDto.fromDto
            let! srt = dto.sorter |> Result.bindOption SorterDto.fromDto
            let! swu = dto.switchUses |> Result.bindOption SwitchUsesDto.fromDto
            let! prf = dto.perf  |> SorterPerfDto.fromDto
            let! e = dto.energy  |> Energy.create ""
            return 
             {
                sorterShcArch.step = st;
                revision = rev;
                rngGen = rng;
                sorter = srt;
                switchUses = swu;
                perf = prf;
                energy = e;
             }
        }
                     
    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<sorterShcArchDto> jstr
            return! fromDto dto
        }

    let toDto (ssA:sorterShcArch) =
            {
                 sorterShcArchDto.step = (StepNumber.value ssA.step);
                 rev = (RevNumber.value ssA.revision);
                 rngGen = ssA.rngGen |> Option.bind (RngGenDto.toDto >> Some);
                 sorter = ssA.sorter |> Option.bind (SorterDto.toDto >> Some);
                 switchUses = ssA.switchUses |> Option.bind (SwitchUsesDto.toDto >> Some);
                 perf = ssA.perf |> SorterPerfDto.toDto
                 energy = (Energy.value ssA.energy); 
           }

    let toJson (idt:sorterShcArch) =
        idt |> toDto |> Json.serialize


type sorterShcSpecDto = 
    {
     rngGen:rngGenDto;
     sorter:sorterDto;
     mutSpec:sorterMutSpecDto;
     srtblStType:sortableSetTypeDto
     stWgtSpec:shcStageWeightSpecDto
     evalSpec:sorterEvalSpecDto
     annealer:annealerSpecDto;
     updater:shcSaveDetailsDto;
     term:shcTermSpecDto; }
module SorterShcSpecDto =
    let fromDto (dto:sorterShcSpecDto) =
        result {
            let! rng = dto.rngGen |> RngGenDto.fromDto
            let! srt = dto.sorter |> SorterDto.fromDto
            let! mutSpec = dto.mutSpec |> SorterMutSpecDto.fromDto
            let! ssRs = dto.srtblStType |> SortableSetTypeDto.fromDto
            let! swS = dto.stWgtSpec |> ShcStageWeightSpecDto.fromDto
            let! evl = dto.evalSpec |> SorterEvalSpecDto.fromDto
            let! ann = dto.annealer  |> AnnealerSpecDto.fromDto
            let! updt = dto.updater  |> ShcSaveDetailsDto.fromDto
            let! term = dto.term  |> ShcTermSpecDto.fromDto
            return 
             {
                sorterShcSpec.rngGen = rng;
                sorter = srt;
                mutatorSpec = mutSpec;
                srtblSetType = ssRs;
                sorterStageWeightSpec = swS;
                evalSpec = evl;
                annealerSpec = ann;
                updaterSpec = updt;
                termSpec = term;
             }
        }
                     
    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<sorterShcSpecDto> jstr
            return! fromDto dto
        }

    let toDto (sss:sorterShcSpec) =
            {
                sorterShcSpecDto.rngGen = sss.rngGen |> RngGenDto.toDto;
                sorter = sss.sorter |> SorterDto.toDto;
                mutSpec = sss.mutatorSpec |> SorterMutSpecDto.toDto
                srtblStType = sss.srtblSetType |> SortableSetTypeDto.toDto
                stWgtSpec = sss.sorterStageWeightSpec |> ShcStageWeightSpecDto.toDto
                evalSpec = sss.evalSpec |> SorterEvalSpecDto.toDto
                annealer = sss.annealerSpec |> AnnealerSpecDto.toDto
                updater = sss.updaterSpec |> ShcSaveDetailsDto.toDto
                term = sss.termSpec  |> ShcTermSpecDto.toDto
            }

    let toJson (idt:sorterShcSpec) =
        idt |> toDto |> Json.serialize




type sssrgTypeDto  = {cat:string; value:string}
module SssrgTypeDto =
    let fromDto (dto:sssrgTypeDto) =
        match dto.cat with
        | nameof sssrgType.Annealer -> 
            result {
                let! spec = dto.value |> ByteUtils.base64ToObj |> Ok
                return sssrgType.Annealer spec
            }
        | nameof sssrgType.Mutation -> 
            result {
                let! spec = dto.value |> ByteUtils.base64ToObj  |> Ok
                return sssrgType.Mutation spec
            }
        | nameof sssrgType.RndGen -> 
            result {
                return sssrgType.RndGen
            }
        | nameof sssrgType.Sorters -> 
            result {
                let! spec = dto.value |> ByteUtils.base64ToObj  |> Ok
                return sssrgType.Sorters spec
            }
        | nameof sssrgType.StageWeight ->
            result {
                let! spec = dto.value |> ByteUtils.base64ToObj  |> Ok
                return sssrgType.StageWeight spec
            }
        | t -> sprintf "cat: %s for sssrgTypeDto not found"
                     dto.cat |> Error
                     

    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<sssrgTypeDto> jstr
            return! fromDto dto
        }


    let toDto (sss:sssrgType) =
        match sss with
        | sssrgType.Annealer anlrs -> 
                { sssrgTypeDto.cat = nameof sssrgType.Annealer; 
                    value= anlrs |> ByteUtils.base64FromObj }
        | sssrgType.Mutation srtMutsp -> 
                { sssrgTypeDto.cat = nameof sssrgType.Mutation; 
                    value= srtMutsp |> ByteUtils.base64FromObj}
        | sssrgType.RndGen -> 
                { sssrgTypeDto.cat = nameof sssrgType.RndGen; 
                  value=""}
        | sssrgType.Sorters ssg -> 
                { sssrgTypeDto.cat = nameof sssrgType.Sorters; 
                  value = ssg |> ByteUtils.base64FromObj}
        | sssrgType.StageWeight swsp -> 
                { sssrgTypeDto.cat = nameof sssrgType.StageWeight; 
                  value = swsp |> ByteUtils.base64FromObj}

    let toJson (idt:sssrgType) =
        idt |> toDto |> Json.serialize


type sorterShcSpecRndGenDto = 
    {
         baseSpec:sorterShcSpecDto;
         sssrgT:sssrgTypeDto;
         rngGen: rngGenDto;
         count:int;
     }
module SorterShcSpecRndGenDto =
    let fromDto (dto:sorterShcSpecRndGenDto) =
        result {
            let! rng = dto.rngGen |> RngGenDto.fromDto
            let! srt = dto.sssrgT |> SssrgTypeDto.fromDto
            let! bs = dto.baseSpec |> SorterShcSpecDto.fromDto
            let! shcCt = dto.count |> ShcCount.create ""
            return 
             {
                sorterShcSpecRndGen.rndGen = rng;
                sorterShcSpecRndGen.baseSpec = bs;
                sorterShcSpecRndGen.count = shcCt;
                sorterShcSpecRndGen.sssrgType = srt;
             }
        }
                     
    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<sorterShcSpecRndGenDto> jstr
            return! fromDto dto
        }

    let toDto (sss:sorterShcSpecRndGen) =
            {
                sorterShcSpecRndGenDto.rngGen = sss.rndGen |> RngGenDto.toDto;
                sorterShcSpecRndGenDto.baseSpec = sss.baseSpec |> SorterShcSpecDto.toDto;
                sorterShcSpecRndGenDto.count = sss.count |> ShcCount.value;
                sorterShcSpecRndGenDto.sssrgT = sss.sssrgType |> SssrgTypeDto.toDto
            }

    let toJson (idt:sorterShcSpecRndGen) =
        idt |> toDto |> Json.serialize


    let fromBase64 (jstr:string) =
        result {
            let! dto = jstr |> ByteUtils.base64ToObj |> Ok
            return dto
        }


    let toBase64 (idt:sorterShcSpecRndGen) =
        idt |> ByteUtils.base64FromObj









type sorterShcResultDto = {
    sorterShc:sorterShcSpecDto; 
    msg:string; 
    archives:sorterShcArchDto[]
    }
module SorterShcResultDto =
    let fromDto (dto:sorterShcResultDto) =
        result {
            let! spec = dto.sorterShc |> SorterShcSpecDto.fromDto
            let! arch = dto.archives |> Array.map(SorterShcArchDto.fromDto)
                                    |> Array.toList
                                    |> Result.sequence
            return {sorterShcResult.spec = spec;
                    sorterShcResult.msg = dto.msg;
                    sorterShcResult.archives = arch |> List.toArray}
        }

    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<sorterShcResultDto> jstr
            return! fromDto dto
        }

    let toDto (ssR:sorterShcResult) =
        { sorterShcResultDto.sorterShc = ssR.spec |> SorterShcSpecDto.toDto;
          msg=ssR.msg;
          archives = ssR.archives |> Array.map(SorterShcArchDto.toDto)}

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

