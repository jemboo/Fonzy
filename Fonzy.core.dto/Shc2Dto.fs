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
         term:shcTermSpecDto; 
     }


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




type sorterShc2Dto = 
    {
        shcId:string;
        sorterId:string;
        generation:int;
        seed:string;
        mut:string;
        temp:string;
        degree:int; 
        generationSpan:int; 
        perfBinsTrial:int[][];
        perfBinsAccepted:int[][];
        switches:int[]
        weights:int[];
        switchesUsed:int; 
        stagesUsed:int; 
        successful:string;
        energy:float;
    }

    
module SorterShc2Dto =

    let toDto (ssA:sorterShc) 
                (id:ShcId) 
                (sorterId:string)
                (seed:string) 
                (mut:string)
                (temp:string)
                (genSpan:int) 
                (prfBnsTrial:SortingEval.sorterPerfBin[])
                (prfBnsAccepted:SortingEval.sorterPerfBin[]) =
        let wgths = 
            match ssA.switchUses with
            | Some uss -> uss.weights
            | None -> [||]
        let switchesUsed = 
            match ssA.perf with
            | Some perf -> perf.usedSwitchCount |> SwitchCount.value
            | None -> 0
        let stagesUsed = 
            match ssA.perf with
            | Some perf -> perf.usedStageCount |> StageCount.value
            | None -> 0
        let successfulSort = 
            match ssA.perf with
            | Some perf -> perf.successful |> BasicDto.toCereal
            | None -> "false"
        let energy = 
            match ssA.energy with
            | Some enrg -> enrg |> Energy.value
            | None -> 1000.0
        { 
            sorterShc2Dto.shcId = (ShcId.value id) |> string;
            sorterId = sorterId;
            generation = (StepNumber.value ssA.step);
            seed = seed;
            mut = mut;
            temp = temp;
            degree = (Degree.value ssA.sorter.degree); 
            generationSpan = genSpan;
            perfBinsTrial = prfBnsTrial |> SorterPerfBinDto.toDtos;
            perfBinsAccepted = prfBnsAccepted |> SorterPerfBinDto.toDtos;
            switches = ssA.sorter.switches |> Array.map(SwitchDto.toDto);
            weights = wgths;
            switchesUsed = switchesUsed; 
            stagesUsed = stagesUsed; 
            successful = successfulSort;
            energy = energy; 
        }

    let toJson (ssA:sorterShc) 
                (id:ShcId) 
                (sorterId:string)
                (seed:string) 
                (mut:string)
                (temp:string)
                (genSpan:int) 
                (prfBnsTrial:SortingEval.sorterPerfBin[])
                (prfBnsAccepted:SortingEval.sorterPerfBin[]) =
        toDto ssA id sorterId seed mut temp genSpan prfBnsTrial prfBnsAccepted
                |> Json.serialize


    let getPerfBinsTrial (dto:sorterShc2Dto) = 
        result {
            return! dto.perfBinsTrial 
                       |> Array.map(SorterPerfBinDto.fromInts)
                       |> Array.toList
                       |> Result.sequence
        }


    let getPerfBinsAccepted (dto:sorterShc2Dto) = 
        result {
            return! dto.perfBinsAccepted 
                       |> Array.map(SorterPerfBinDto.fromInts)
                       |> Array.toList
                       |> Result.sequence
        }





type sorterShcMergedDto = 
    {
        mergeCt:int;
        sorterId:string;
        generation:int;
        mut:string;
        temp:string;
        degree:int; 
        generationSpan:int; 
        perfBinsTrial:int[][];
        perfBinsAccepted:int[][];
        perfBinsCurrent:int[][];
    }


module SorterShcMergedDto =
    let makeEmpty (sorterId:string) 
                  (generation:int)
                  (mut:string)
                  (temp:string)
                  (degree:int)
                  (generationSpan:int) =
        {
            sorterShcMergedDto.mergeCt = 0
            sorterId = sorterId
            generation = generation
            mut = mut 
            temp = temp
            degree = degree
            generationSpan = generationSpan
            perfBinsTrial = Array.empty
            perfBinsAccepted = Array.empty
            perfBinsCurrent = Array.empty
        }


    let merge (a:sorterShc2Dto seq) =
        let allin = a |> Seq.toArray
        let _toSorterPerf (dto:sorterShc2Dto) =
            {
                SortingEval.sorterPerf.usedSwitchCount = (SwitchCount.fromInt dto.switchesUsed)
                SortingEval.sorterPerf.usedStageCount = (StageCount.fromInt dto.stagesUsed)
                SortingEval.sorterPerf.successful = Some (dto.successful |> bool.Parse)

            }

        let _trialSorterPerfBins (dtos:sorterShc2Dto seq) =
            result {
                let! lols = dtos |> Seq.map(SorterShc2Dto.getPerfBinsTrial)
                                |> Seq.toList
                                |> Result.sequence

                let mergedBins = lols |> Seq.concat
                                      |> SortingEval.SorterPerfBin.merge
                return mergedBins |> Seq.map(SorterPerfBinDto.toDto)
                                  |> Seq.toArray
            }
   
   
        let _acceptedSorterPerfBins (dtos:sorterShc2Dto seq) =
            result {
                   let! lols = dtos |> Seq.map(SorterShc2Dto.getPerfBinsAccepted)
                                   |> Seq.toList
                                   |> Result.sequence

                   let mergedBins = lols |> Seq.concat
                                         |> SortingEval.SorterPerfBin.merge
                   return mergedBins |> Seq.map(SorterPerfBinDto.toDto)
                                     |> Seq.toArray
               }


        let perfBinsCurrent = allin |> Array.map (_toSorterPerf)
                              |> SortingEval.SorterPerfBin.fromSorterPerfs

        result {

            let! acceptedBins =  _acceptedSorterPerfBins allin
            let! trialBins = _trialSorterPerfBins allin
        
            return {
                sorterShcMergedDto.mergeCt = allin.Length;
                sorterId = allin.[0].sorterId;
                generation = allin.[0].generation;
                mut = allin.[0].mut;
                temp = allin.[0].temp;
                degree = allin.[0].degree;
                generationSpan = allin.[0].generationSpan;
                perfBinsTrial =  trialBins;
                perfBinsAccepted = acceptedBins;
                perfBinsCurrent = perfBinsCurrent |> SorterPerfBinDto.toDtos;
            }
         }

