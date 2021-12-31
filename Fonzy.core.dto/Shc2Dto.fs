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
        lastSwitchUsed:int
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
            lastSwitchUsed = ssA.lastSwitchUsed |> SwitchCount.value;
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
        dto.perfBinsTrial |> SorterPerfBinDto.fromIntArrays


    let getPerfBinsAccepted (dto:sorterShc2Dto) = 
        dto.perfBinsAccepted |> SorterPerfBinDto.fromIntArrays




type sorterShcMergedDto = 
    {
        mergeCt:int;
        sorterId:string;
        generation:int;
        mut:string;
        temp:string;
        degree:int; 
        generationSpan:int;
        switchUseEntropy:double;
        switchAction:double;
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
            switchUseEntropy = 0.0;
            switchAction = 0.0;
            perfBinsTrial = Array.empty
            perfBinsAccepted = Array.empty
            perfBinsCurrent = Array.empty
        }



    let getPerfBinsTrial (dto:sorterShcMergedDto) = 
        dto.perfBinsTrial |> SorterPerfBinDto.fromIntArrays


    let getPerfBinsAccepted (dto:sorterShcMergedDto) = 
        dto.perfBinsAccepted |> SorterPerfBinDto.fromIntArrays


    let getPerfBinsCurrent (dto:sorterShcMergedDto) = 
        dto.perfBinsCurrent |> SorterPerfBinDto.fromIntArrays


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

        let _getEntropyAndAction(dto:sorterShc2Dto) =
            let _net (actn:float) (sw:float) =
                if sw = 0 then 0.0 else (sw/actn) * Math.Log(sw/actn)
            let action = dto.weights |> Array.sum |> float
            let ntropy = dto.weights |> Array.map(float >> (_net action))  |> Array.sum 
            (-ntropy, action)

        let _getEntropyAndActionAve(dtos:sorterShc2Dto[]) =
            let eas = dtos |> Array.map(_getEntropyAndAction)
            (eas |> Array.map(fst) |>Array.average,
             eas |> Array.map(snd) |>Array.average)

        let perfBinsCurrent = allin |> Array.map (_toSorterPerf)
                              |> SortingEval.SorterPerfBin.fromSorterPerfs
        let action = allin |> Array.map(fun p -> p.weights |> Array.sum |> float)
                           |> Array.average
        let e, a =  allin |> _getEntropyAndActionAve
        result {

            let! acceptedBins =  _acceptedSorterPerfBins allin
            let! trialBins = _trialSorterPerfBins allin
        
            return {
                sorterShcMergedDto.mergeCt = allin.Length;
                sorterId = allin.[0].sorterId;
                generation = allin.[0].generation;
                switchAction = a;
                switchUseEntropy = e;
                mut = allin.[0].mut;
                temp = allin.[0].temp;
                degree = allin.[0].degree;
                generationSpan = allin.[0].generationSpan;
                perfBinsTrial =  trialBins;
                perfBinsAccepted = acceptedBins;
                perfBinsCurrent = perfBinsCurrent |> SorterPerfBinDto.toDtos;
            }
         }



    let pivotTableHdrs = "mergeCt\t srtrId\t gen\t mut\t temp\t" +
                         "action\t entropy\t degree\t genSpan\t" +
                         "trlMinE\t trlMeanE\t trlMaxE\t trlStdE\t" +
                         "accptMinE\t accptMeanE\t accptMaxE\t accptStdE\t" +
                         "curMinE\t curMeanE\t curMaxE\t curStdE\t" +
                         "trlMinW\t trlMeanW\t trlMaxW\t trlStdW\t" +
                         "accptMinW\t accptMeanW\t accptMaxW\t accptStdW\t" +
                         "curMinW\t curMeanW\t curMaxW\t curStdW\t" +
                         "trlMinT\t trlMeanT\t trlMaxT\t trlStdT\t" +
                         "accptMinT\t accptMeanT\t accptMaxT\t accptStdT\t" +
                         "curMinT\t curMeanT\t curMaxT\t curStdT\t"


    let toReport (energyF:SortingEval.sorterPerfBin->double) 
                 (mDto:sorterShcMergedDto)  =

        let _getSwitches (bin:SortingEval.sorterPerfBin) = 
                bin.usedSwitchCount |> SwitchCount.value |> float
        let _getStages (bin:SortingEval.sorterPerfBin) = 
                bin.usedStageCount |> StageCount.value |> float

        result {
            let mergeCt = mDto.mergeCt |> string
            let srtrId = mDto.sorterId
            let gen = mDto.generation |> string
            let mut = mDto.mut
            let temp = mDto.temp
            let action = mDto.switchAction |> string
            let entropy = mDto.switchUseEntropy |> string
            let degree = mDto.degree |> string
            let genSpan = mDto.generationSpan |> string
            let chunk1 = sprintf "%s\t%s\t%s\t%s\t%s" mergeCt srtrId gen mut temp
            let chunk2 = sprintf "%s\t%s\t%s\t%s"  action entropy degree genSpan

            let! trialBins =  mDto |> getPerfBinsTrial
            let! acceptedBins =  mDto |> getPerfBinsAccepted
            let! currentBins =  mDto |> getPerfBinsCurrent


            let trlMinE, trlMaxE, trlMeanE =  trialBins |> SortingEval.SorterPerfBin.getMinMaxMeanOfSuccessful energyF
            let trlStdE = trialBins |> SortingEval.SorterPerfBin.getStdevOfSuccessful energyF trlMeanE
            let chunk3 = sprintf "%s\t%s\t%s\t%s" (string trlMinE) (string trlMeanE) (string trlMaxE) (string trlStdE) 


            let accptMinE, accptMaxE, accptMeanE =  acceptedBins |> SortingEval.SorterPerfBin.getMinMaxMeanOfSuccessful energyF
            let accptStdE = acceptedBins |> SortingEval.SorterPerfBin.getStdevOfSuccessful energyF accptMeanE
            let chunk4 = sprintf "%s\t%s\t%s\t%s" (string accptMinE) (string accptMeanE) (string accptMaxE) (string accptStdE)
        

            let curMinE, curMaxE, curMeanE =  currentBins |> SortingEval.SorterPerfBin.getMinMaxMeanOfSuccessful energyF
            let curStdE = currentBins |> SortingEval.SorterPerfBin.getStdevOfSuccessful energyF curMeanE
            let chunk5 = sprintf "%s\t%s\t%s\t%s" (string curMinE) (string curMeanE) (string curMaxE) (string curStdE)
 

            let trlMinW, trlMaxW, trlMeanW =  trialBins |> SortingEval.SorterPerfBin.getMinMaxMeanOfSuccessful _getSwitches
            let trlStdW = trialBins |> SortingEval.SorterPerfBin.getStdevOfSuccessful _getSwitches trlMeanW
            let chunk6 = sprintf "%s\t%s\t%s\t%s" (string trlMinW) (string trlMeanW) (string trlMaxW) (string trlStdW)
        

            let accptMinW, accptMaxW, accptMeanW =  acceptedBins |> SortingEval.SorterPerfBin.getMinMaxMeanOfSuccessful _getSwitches
            let accptStdW = acceptedBins |> SortingEval.SorterPerfBin.getStdevOfSuccessful _getSwitches accptMeanW
            let chunk7 = sprintf "%s\t%s\t%s\t%s" (string accptMinW) (string accptMeanW) (string accptMaxW) (string accptStdW)
        

            let curMinW, curMaxW, curMeanW =  currentBins |> SortingEval.SorterPerfBin.getMinMaxMeanOfSuccessful _getSwitches
            let curStdW = currentBins |> SortingEval.SorterPerfBin.getStdevOfSuccessful _getSwitches curMeanW
            let chunk8 = sprintf "%s\t%s\t%s\t%s" (string curMinW) (string curMeanW) (string curMaxW) (string curStdW)
        

            let trlMinT, trlMaxT, trlMeanT =  trialBins |> SortingEval.SorterPerfBin.getMinMaxMeanOfSuccessful _getStages
            let trlStdT = trialBins |> SortingEval.SorterPerfBin.getStdevOfSuccessful _getStages trlMeanT
            let chunk9 = sprintf "%s\t%s\t%s\t%s" (string trlMinT) (string trlMeanT) (string trlMaxT) (string trlStdT)
        

            let accptMinT, accptMaxT, accptMeanT =  acceptedBins |> SortingEval.SorterPerfBin.getMinMaxMeanOfSuccessful _getStages
            let accptStdT = acceptedBins |> SortingEval.SorterPerfBin.getStdevOfSuccessful _getStages accptMeanT
            let chunk10 = sprintf "%s\t%s\t%s\t%s" (string accptMinT) (string accptMeanT) (string accptMaxT) (string accptStdT)
        

            let curMinT, curMaxT, curMeanT =  currentBins |> SortingEval.SorterPerfBin.getMinMaxMeanOfSuccessful _getStages
            let curStdT = currentBins |> SortingEval.SorterPerfBin.getStdevOfSuccessful _getStages curMeanT
            let chunk11 = sprintf "%s\t%s\t%s\t%s" (string curMinT) (string curMeanT) (string curMaxT) (string curStdT)


            return sprintf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s" 
                            chunk1 chunk2 chunk3 chunk4 chunk5 chunk6 chunk7 chunk8 chunk9 chunk10 chunk11
        }


