﻿namespace global
open System


type causeSpec = {id:CauseSpecId; genus:string list; prams:stringMapDto;}


module CauseSpec = 
    let tupOp tup op =
        (fst tup, op (snd tup))


    let noOpCauseSpecId = Guid.Parse "00000000-0000-0000-0000-000000000000"
    let noOpCauseSpec = { causeSpec.id = CauseSpecId.fromGuid noOpCauseSpecId; 
                          genus=["NoOp"]; prams={stringMapDto.vals = [||]};}


module CauseSpecRandGen = 
    let intArrayBaseId = Guid.Parse "00000000-0000-0000-0000-000000000001"
    let intArray (idt:intDistType) (count:int) 
                 (rndGen:RngGen) (outName:string) =
        let id = seq { intArrayBaseId:> obj;
                       idt:> obj; count:> obj; 
                       rndGen:> obj; outName:> obj; } 
                        |> GuidUtils.guidFromObjs
        let prams = [
                         ("count", (string count)); 
                         ("rngGen", rndGen |> RngGenDto.toJson);
                         ("intDistType", idt |> IntDistTypeDto.toJson);
                         ("outName", outName)
                     ] 
                     |> Map.ofList
                     |> StringMapDto.toDto

        {causeSpec.id = CauseSpecId.fromGuid id; genus=["RandGen"; "IntArray"]; prams=prams; }

    let int2dArray (idt:int2dDistType) (count:int) 
                          (rndGen:RngGen) (outName:string) =
        let id = seq { intArrayBaseId:> obj;
                       idt:> obj; count:> obj; 
                       rndGen:> obj; outName:> obj; } 
                        |> GuidUtils.guidFromObjs
        let prams = [
                         ("count", (string count)); 
                         ("rngGen", rndGen |> RngGenDto.toJson);
                         ("lattice2dDistType", idt |> Int2dDistTypeDto.toJson);
                         ("outName", outName)
                    ] 
                     |> Map.ofList
                     |> StringMapDto.toDto
        {causeSpec.id = CauseSpecId.fromGuid id; genus=["RandGen"; "Int2dArray"]; prams=prams;}

    let uniformInts (minVal:int) (maxVal:int) 
                    (seed:RandomSeed) (count:int) (outName:string) =
        let idt = intDistType.Uniform {uniformIntegerDistParams.min=minVal; max = maxVal;}
        let rng = RngGen.createLcg seed
        intArray idt count rng outName

    let normalInts (mean:float) (stdev:float) 
                   (seed:RandomSeed) (count:int) (outName:string) =
        let idt = intDistType.Normal {normalIntegerDistParams.mean = mean; stdDev = stdev;}
        let rng = RngGen.createLcg seed
        intArray idt count rng outName

    let uniformInt2dArray (valSpan:int) (seed:RandomSeed) 
                          (count:int) (outName:string) =
        let idt = int2dDistType.Uniform (UniformInt2dDistParams.square valSpan)
        let rng = RngGen.createLcg seed
        int2dArray idt count rng outName

    let normalInt2dArray (stdev:float) (seed:RandomSeed) 
                         (count:int) (outName:string) =
        let idt = int2dDistType.Normal (NormalInt2dDistParams.round stdev)
        let rng = RngGen.createLcg seed
        int2dArray idt count rng outName


module CauseSpecSorters =
    let rndSortersBaseId = Guid.Parse "00000000-0000-0000-0000-000000000002"

    let rndGen (sorterSetId:string*SorterSetId) 
               (sorterGen:string*sorterRndGen)
               (sorterCount:string*SorterCount)
               (rndGen:string*RngGen) 
               (sorterSetName:string*string) =

        let id = seq { rndSortersBaseId:> obj;
                       sorterGen:> obj;
                       rndGen:> obj; 
                       sorterSetName:> obj; } 
                        |> GuidUtils.guidFromObjs
        let prams = [
                         (CauseSpec.tupOp sorterSetId (SorterSetId.value >> string));
                         (CauseSpec.tupOp sorterGen SorterRndGenDto.toJson);
                         (CauseSpec.tupOp sorterCount (SorterCount.value >> string));
                         (CauseSpec.tupOp rndGen RngGenDto.toJson);
                         sorterSetName
                    ] 
                     |> Map.ofList
                     |> StringMapDto.toDto
        {
            causeSpec.id = CauseSpecId.fromGuid id; 
            genus=["Sorters"; "rndGen"]; 
            prams=prams;
        }

    
    let evalSortersBaseId = Guid.Parse "00000000-0000-0000-0000-000000000003"
    let evalToSorterPerfBins 
              (degree:string*Degree)
              (sorterSetName:string*string)
              (switchUsePlan:string*Sorting.switchUsePlan)
              (sortableSet:string*sortableSetType)
              (sorterSaving:string*sorterSaving)
              (useParallel:string*bool)
              (resultsName:string*string) =
        let id = seq { evalSortersBaseId:> obj;
                       degree:> obj;
                       sorterSetName:> obj;
                       switchUsePlan:> obj; 
                       sortableSet:> obj;
                       resultsName:> obj; } 
                        |> GuidUtils.guidFromObjs
        let prams = [
                         (CauseSpec.tupOp degree (Degree.value >> string));
                         sorterSetName;
                         (CauseSpec.tupOp switchUsePlan Json.serialize);
                         (CauseSpec.tupOp sortableSet SortableSetTypeDto.toJson);
                         (CauseSpec.tupOp sorterSaving SorterSavingDto.toJson);
                         (CauseSpec.tupOp useParallel Json.serialize);
                         resultsName
                    ] 
                    |> Map.ofList
                    |> StringMapDto.toDto
        {
            causeSpec.id = CauseSpecId.fromGuid id; 
            genus=["Sorters"; "evalToSorterPerfBins"]; 
            prams=prams;
        }


    let genToPerfBinsBaseId = Guid.Parse "00000000-0000-0000-0000-000000000004"
    let genToSorterPerfBins 
              (sorterGen:string*sorterRndGen)
              (sorterCount:string*SorterCount)
              (rndGen:string*RngGen) 
              (switchUsePlan:string*Sorting.switchUsePlan)
              (sortableSet:string*sortableSetType)
              (sorterSaving:string*sorterSaving)
              (useParallel:string*bool)
              (resultsName:string*string) =

        let id = seq { genToPerfBinsBaseId:> obj;
                       sorterGen:> obj;
                       sorterCount:> obj;
                       rndGen:> obj;
                       switchUsePlan:> obj;
                       sortableSet:> obj;
                       resultsName:> obj; } 
                        |> GuidUtils.guidFromObjs
        let prams = [
                         (CauseSpec.tupOp sorterGen SorterRndGenDto.toJson);
                         (CauseSpec.tupOp sorterCount (SorterCount.value >> string));
                         (CauseSpec.tupOp rndGen RngGenDto.toJson);
                         (CauseSpec.tupOp switchUsePlan Json.serialize);
                         (CauseSpec.tupOp sortableSet SortableSetTypeDto.toJson);
                         (CauseSpec.tupOp sorterSaving SorterSavingDto.toJson);
                         (CauseSpec.tupOp useParallel Json.serialize);
                         resultsName
                     ] 
                     |> Map.ofList
                     |> StringMapDto.toDto
        {
            causeSpec.id = CauseSpecId.fromGuid id; 
            genus=["Sorters"; "genToSorterPerfBins"]; 
            prams=prams;
        }


    let rndGenToPerfBinsBaseId = Guid.Parse "00000000-0000-0000-0000-000000000005"
    let rndGenToPerfBins 
              (sorterRndGen:string*sorterRndGen)
              (sorterCount:string*SorterCount)
              (rndGen:string*RngGen) 
              (switchUsePlan:string*Sorting.switchUsePlan)
              (sortableSet:string*sortableSetType)
              (sorterSaving:string*sorterSaving)
              (useParallel:string*bool)
              (resultsName:string*string) =

        let id = seq { rndGenToPerfBinsBaseId:> obj;
                       sorterRndGen:> obj;
                       sorterCount:> obj;
                       rndGen:> obj;
                       switchUsePlan:> obj;
                       sortableSet:> obj;
                       useParallel:> obj;
                       resultsName:> obj; } 
                        |> GuidUtils.guidFromObjs
        let prams = [
                         (CauseSpec.tupOp sorterRndGen SorterRndGenDto.toJson);
                         (CauseSpec.tupOp sorterCount (SorterCount.value >> string));
                         (CauseSpec.tupOp rndGen RngGenDto.toJson);
                         (CauseSpec.tupOp switchUsePlan Json.serialize);
                         (CauseSpec.tupOp sortableSet SortableSetTypeDto.toJson);
                         (CauseSpec.tupOp sorterSaving SorterSavingDto.toJson);
                         (CauseSpec.tupOp useParallel Json.serialize);
                         resultsName
                     ] 
                     |> Map.ofList
                     |> StringMapDto.toDto
        {
            causeSpec.id = CauseSpecId.fromGuid id; 
            genus=["Sorters"; "rndGenToPerfBins"]; 
            prams=prams;
        }

        
module CauseSpecSorterShc =

    let sorterShcSpecRndGenBaseId = Guid.Parse "00000000-0000-0000-0000-000000000006"
    let sorterShcSpecRndGen
        (sorterShcSpecRndGen:string*sorterShcSpecRndGen)
        (useParallel:string*bool) 
        (resultsName:string*string) =

        let id = seq { sorterShcSpecRndGenBaseId:> obj;
                        sorterShcSpecRndGen:> obj;
                        useParallel:> obj;
                        resultsName:> obj; } 
                        |> GuidUtils.guidFromObjs

        let prams = [ 
                        (CauseSpec.tupOp sorterShcSpecRndGen SorterShcSpecRndGenDto.toJson);
                        (CauseSpec.tupOp useParallel Json.serialize);
                        resultsName
                    ] 
                    |> Map.ofList
                    |> StringMapDto.toDto
        {
            causeSpec.id = CauseSpecId.fromGuid id; 
            genus=["SorterShc"; "sorterShcSpecRndGen"]; 
            prams=prams;
        }


    let sorterShcSpecRndGen2
        (sorterShcSpecRndGen:string*sorterShcSpecRndGen2)
        (useParallel:string*bool) 
        (resultsName:string*string) =

        let id = seq { sorterShcSpecRndGenBaseId:> obj;
                        sorterShcSpecRndGen:> obj;
                        useParallel:> obj;
                        resultsName:> obj; } 
                        |> GuidUtils.guidFromObjs

        let prams = [ 
                        (CauseSpec.tupOp sorterShcSpecRndGen SorterShcSpecRndGen2Dto.toJson);
                        (CauseSpec.tupOp useParallel Json.serialize);
                        resultsName
                    ] 
                    |> Map.ofList
                    |> StringMapDto.toDto
        {
            causeSpec.id = CauseSpecId.fromGuid id; 
            genus=["SorterShc"; "sorterShcSpecRndGen2"]; 
            prams=prams;
        }










        
        
            //let lookupKeyedInt<'a> (key:string) 
            //                       (cs:CauseSpec) : Result<int, string> =
            //    match cs.prams.TryFind key with
            //    | Some value -> 
            //      try
            //        (value |> int) |> Ok
            //      with
            //      | :? InvalidCastException ->
            //        sprintf "value: %s could not be cast to int" value |> Error
            
            //    | None -> sprintf "key not found: %s" key|> Error
        
            //let lookupKeyedFloat<'a> (key:string) 
            //                         (cs:CauseSpec) : Result<float, string> =
            //    match cs.prams.TryFind key with
            //    | Some value -> 
            //      try
            //        (value |> float) |> Ok
            //      with
            //      | :? InvalidCastException ->
            //        sprintf "value: %s could not be cast to float" value |> Error
        
            //    | None -> sprintf "key not found: %s" key|> Error
        
        
            //let lookupKeyedBool<'a> (key:string) 
            //                        (cs:CauseSpec) : Result<bool, string> =
            //    match cs.prams.TryFind key with
            //    | Some value -> 
            //      try
            //        (value |> bool.Parse) |> Ok
            //      with
            //      | :? InvalidCastException ->
            //        sprintf "value: %s could not be cast to bool" value |> Error
        
            //    | None -> sprintf "key not found: %s" key|> Error
        
        
            //let procKeyedInt<'a> (key:string) (proc:int->Result<'a,string>) 
            //                     (cs:CauseSpec) : Result<'a, string> =
            //    result {
            //        let! iv = lookupKeyedInt key cs
            //        return! proc iv
            //    }
        
            //let procKeyedFloat<'a> (key:string) (proc:float->Result<'a,string>) 
            //                       (cs:CauseSpec) : Result<'a, string> =
            //    result {
            //        let! iv = lookupKeyedFloat key cs
            //        return! proc iv
            //    }
        
            //let procKeyedJson<'a> (key:string) (proc:string->Result<'a,string>) 
            //                     (cs:CauseSpec) : Result<'a, string> =
            //    result {
            //        let! cereal = ResultMap.read key cs.prams
            //        return! proc cereal
            //    }