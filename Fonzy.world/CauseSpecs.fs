﻿namespace global
open System


type CauseSpec = {id:Guid; genus:string list; prams:Map<string,string>;}

module CauseSpecRandGen = 
    let intArrayBaseId = Guid.Parse "00000000-0000-0000-0000-000000000001"
    let intArray (idt:IntDistType) (count:int) 
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
                     ] |> Map.ofList
        {CauseSpec.id = id; genus=["RandGen"; "IntArray"]; prams=prams; }

    let int2dArray (idt:Int2dDistType) (count:int) 
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
                     ] |> Map.ofList
        {CauseSpec.id = id; genus=["RandGen"; "Int2dArray"]; prams=prams;}

    let uniformInts (minVal:int) (maxVal:int) 
                    (seed:int) (count:int) (outName:string) =
        let idt = IntDistType.Uniform {UniformIntegerDistParams.min=minVal; max = maxVal;}
        let rng = RngGen.createLcg seed
        intArray idt count rng outName

    let normalInts (mean:float) (stdev:float) 
                   (seed:int) (count:int) (outName:string) =
        let idt = IntDistType.Normal {NormalIntegerDistParams.mean = mean; stdDev = stdev;}
        let rng = RngGen.createLcg seed
        intArray idt count rng outName

    let uniformInt2dArray (valSpan:int) (seed:int) 
                          (count:int) (outName:string) =
        let idt = Int2dDistType.Uniform (UniformInt2dDistParams.square valSpan)
        let rng = RngGen.createLcg seed
        int2dArray idt count rng outName

    let normalInt2dArray (stdev:float) (seed:int) 
                         (count:int) (outName:string) =
        let idt = Int2dDistType.Normal (NormalInt2dDistParams.round stdev)
        let rng = RngGen.createLcg seed
        int2dArray idt count rng outName


module CauseSpecSorters =
    let rndSortersBaseId = Guid.Parse "00000000-0000-0000-0000-000000000002"

    let rndGen (degree:Degree) (sorterLength:SorterLength) 
               (switchFreq:SwitchFrequency) (sorterCount:SorterCount)
               (rndGen:RngGen) (outName:string) =
        let id = seq { rndSortersBaseId:> obj;
                       degree:> obj; sorterLength:> obj;
                       switchFreq:> obj;
                       rndGen:> obj; outName:> obj; } 
                        |> GuidUtils.guidFromObjs
        let prams = [
                     ("degree", degree |> Degree.value |> string);
                     ("sorterLength", sorterLength |> SorterLengthDto.toJson);
                     ("switchFreq", switchFreq |> SwitchFrequency.value |> string);
                     ("sorterCount", sorterCount |> SorterCount.value |> string);
                     ("rngGen", rndGen |> RngGenDto.toJson);
                     ("outName", outName)
                     ] |> Map.ofList
        {CauseSpec.id = id; genus=["Sorters"; "rndGen"]; prams=prams;}


module CauseSpec = 
    let lookupKeyedInt<'a> (key:string) 
                           (cs:CauseSpec) : Result<int, string> =
        match cs.prams.TryFind key with
        | Some value -> 
          try
            (value |> int) |> Ok
          with
          | :? InvalidCastException ->
            sprintf "value: %s could not be cast to int" value |> Error
    
        | None -> sprintf "key not found: %s" key|> Error

    let lookupKeyedFloat<'a> (key:string) 
                             (cs:CauseSpec) : Result<float, string> =
        match cs.prams.TryFind key with
        | Some value -> 
          try
            (value |> float) |> Ok
          with
          | :? InvalidCastException ->
            sprintf "value: %s could not be cast to float" value |> Error

        | None -> sprintf "key not found: %s" key|> Error


    let lookupKeyedBool<'a> (key:string) 
                            (cs:CauseSpec) : Result<bool, string> =
        match cs.prams.TryFind key with
        | Some value -> 
          try
            (value |> bool.Parse) |> Ok
          with
          | :? InvalidCastException ->
            sprintf "value: %s could not be cast to bool" value |> Error

        | None -> sprintf "key not found: %s" key|> Error


    let procKeyedInt<'a> (key:string) (proc:int->Result<'a,string>) 
                         (cs:CauseSpec) : Result<'a, string> =
        result {
            let! iv = lookupKeyedInt key cs
            return! proc iv
        }

    let procKeyedFloat<'a> (key:string) (proc:float->Result<'a,string>) 
                           (cs:CauseSpec) : Result<'a, string> =
        result {
            let! iv = lookupKeyedFloat key cs
            return! proc iv
        }

    let procKeyedJson<'a> (key:string) (proc:string->Result<'a,string>) 
                         (cs:CauseSpec) : Result<'a, string> =
        result {
            let! cereal = ResultMap.read key cs.prams
            return! proc cereal
        }

    let noOpCauseSpecId = Guid.Parse "00000000-0000-0000-0000-000000000000"
    let noOpCauseSpec = { CauseSpec.id = noOpCauseSpecId; 
                          genus=["NoOp"]; prams=Map.empty;}