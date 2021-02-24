namespace global
open System


type CauseSpec = {id:Guid; genus:string list; prams:Map<string,string>;}

module CauseSpecRandGen = 
    let intArray (idt:IntDistType) (count:int) 
                 (rndGen:RngGen) (outName:string) (id:Guid) =
        let prams = [
                     ("count", (string count)); 
                     ("rngGen", rndGen |> RngGenDto.toJson);
                     ("intDistType", idt |> IntDistTypeDto.toJson);
                     ("outName", outName)
                     ] |> Map.ofList
        {CauseSpec.id = id; genus=["RandGen"; "IntArray"]; prams=prams; }

    let latticeLoc2dArray (idt:Lattice2dDistType) (count:int) 
                          (rndGen:RngGen) (outName:string) (id:Guid) =
        let prams = [
                     ("count", (string count)); 
                     ("rngGen", rndGen |> RngGenDto.toJson);
                     ("lattice2dDistType", idt |> Lattice2dDistTypeDto.toJson);
                     ("outName", outName)
                     ] |> Map.ofList
        {CauseSpec.id = id; genus=["RandGen"; "LatticeLoc2dArray"]; prams=prams;}

    let uniformInts (minVal:int) (maxVal:int) 
                    (seed:int) (count:int) (outName:string) (id:Guid) =
        let idt = IntDistType.Uniform {UniformIntegerDistParams.min=minVal; max = maxVal;}
        let rng = RngGen.createLcg seed
        intArray idt count rng outName id

    let normalInts (mean:float) (stdev:float) 
                   (seed:int) (count:int) (outName:string) (id:Guid) =
        let idt = IntDistType.Normal {NormalIntegerDistParams.mean = mean; stdDev = stdev;}
        let rng = RngGen.createLcg seed
        intArray idt count rng outName id

    let uniformLatticeLoc2dArray (valSpan:int) (seed:int) 
                                 (count:int) (outName:string) (id:Guid) =
        let idt = Lattice2dDistType.Uniform (UniformLattice2dDistParams.square valSpan)
        let rng = RngGen.createLcg seed
        latticeLoc2dArray idt count rng outName id

    let normalLatticeLoc2dArray (stdev:float) (seed:int) 
                                (count:int) (outName:string) (id:Guid) =
        let idt = Lattice2dDistType.Normal (NormalLattice2dDistParams.round stdev)
        let rng = RngGen.createLcg seed
        latticeLoc2dArray idt count rng outName id

module CauseSpecSorters =
    let testCauseSpec1Id = Guid.Parse "Sorters0-0000-0000-0000-000000000001"
    let testCauseSpec1 = { CauseSpec.id = testCauseSpec1Id; 
                           genus=["Sorters"; "1"]; prams=Map.empty;}

    let testCauseSpec2Id = Guid.Parse "Sorters0-0000-0000-0000-000000000002"
    let testCauseSpec2 = { CauseSpec.id = testCauseSpec2Id; 
                           genus=["Sorters"; "2"]; prams=Map.empty;}

    let testCauseSpec3Id = Guid.Parse "Sorters0-0000-0000-0000-000000000003"
    let testCauseSpec3 = { CauseSpec.id = testCauseSpec3Id; 
                           genus=["Sorters"; "3"]; prams=Map.empty;}

module CauseSpec = 
    let noOpCauseSpecId = Guid.Parse "00000000-0000-0000-0000-000000000000"
    let noOpCauseSpec = { CauseSpec.id = noOpCauseSpecId; 
                          genus=["NoOp"]; prams=Map.empty;}