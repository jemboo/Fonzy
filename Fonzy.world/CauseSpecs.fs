namespace global
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


    let testCauseSpec1Id = Guid.Parse "AAAAAAAA-0000-0000-0000-000000000001"
    let testCauseSpec1 = { CauseSpec.id = testCauseSpec1Id; 
                           genus=["Sorters"; "1"]; prams=Map.empty;}

    let testCauseSpec2Id = Guid.Parse "AAAAAAAA-0000-0000-0000-000000000002"
    let testCauseSpec2 = { CauseSpec.id = testCauseSpec2Id; 
                           genus=["Sorters"; "2"]; prams=Map.empty;}

    let testCauseSpec3Id = Guid.Parse "AAAAAAAA-0000-0000-0000-000000000003"
    let testCauseSpec3 = { CauseSpec.id = testCauseSpec3Id; 
                           genus=["Sorters"; "3"]; prams=Map.empty;}

module CauseSpec = 
    let noOpCauseSpecId = Guid.Parse "00000000-0000-0000-0000-000000000000"
    let noOpCauseSpec = { CauseSpec.id = noOpCauseSpecId; 
                          genus=["NoOp"]; prams=Map.empty;}