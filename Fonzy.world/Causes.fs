namespace global
open System



type CauseSpec = {id:Guid; genus:string list; prams:Map<string,string>; keyMap:Map<string,string>}

type Cause = {causeSpec:CauseSpec; op:Enviro -> Result<Enviro, string>}

module CauseSpecRandGen = 
    let intArray (idt:IntDistType) (count:int) (rndGen:RngGen) (outName:string) (id:Guid) =
        let prams = [
                     ("count", (string count)); 
                     ("rngGen", rndGen |> RngGenDto.toJson);
                     ("intDistType", idt |> IntDistTypeDto.toJson);
                     ("outName", outName)
                     ] |> Map.ofList
        {CauseSpec.id = id; genus=["RandGen"; "IntArray"]; prams=prams; keyMap=Map.empty;}

    let latticeLoc2dArray (idt:Lattice2dDistType) (count:int) (rndGen:RngGen) (outName:string) (id:Guid) =
        let prams = [
                     ("count", (string count)); 
                     ("rngGen", rndGen |> RngGenDto.toJson);
                     ("lattice2dDistType", idt |> Lattice2dDistTypeDto.toJson);
                     ("outName", outName)
                     ] |> Map.ofList
        {CauseSpec.id = id; genus=["RandGen"; "LatticeLoc2dArray"]; prams=prams; keyMap=Map.empty;}

    let uniformInts (minVal:int) (maxVal:int) (seed:int) (count:int) (outName:string) (id:Guid) =
        let idt = IntDistType.Uniform {UniformIntegerDistParams.min=minVal; max = maxVal;}
        let rng = RngGen.createLcg seed
        intArray idt count rng outName id

    let normalInts (mean:float) (stdev:float) (seed:int) (count:int) (outName:string) (id:Guid) =
        let idt = IntDistType.Normal {NormalIntegerDistParams.mean = mean; stdDev = stdev;}
        let rng = RngGen.createLcg seed
        intArray idt count rng outName id

    let uniformLatticeLoc2dArray (valSpan:int) (seed:int) (count:int) (outName:string) (id:Guid) =
        let idt = Lattice2dDistType.Uniform (UniformLattice2dDistParams.square valSpan)
        let rng = RngGen.createLcg seed
        latticeLoc2dArray idt count rng outName id

    let normalLatticeLoc2dArray (stdev:float) (seed:int) (count:int) (outName:string) (id:Guid) =
        let idt = Lattice2dDistType.Normal (NormalLattice2dDistParams.round stdev)
        let rng = RngGen.createLcg seed
        latticeLoc2dArray idt count rng outName id

module CauseSpec = 
    let noOpCauseSpecId = Guid.Parse "00000000-0000-0000-0000-000000000000"
    let noOpCauseSpec = { CauseSpec.id = noOpCauseSpecId; 
                          genus=["NoOp"]; prams=Map.empty; keyMap=Map.empty;}

    let testCauseSpec1Id = Guid.Parse "00000000-0000-0000-0000-000000000001"
    let testCauseSpec1 = { CauseSpec.id = testCauseSpec1Id; 
                           genus=["Test"; "1"]; prams=Map.empty; keyMap=Map.empty;}

    let testCauseSpec2Id = Guid.Parse "00000000-0000-0000-0000-000000000002"
    let testCauseSpec2 = { CauseSpec.id = testCauseSpec2Id; 
                           genus=["Test"; "2"]; prams=Map.empty; keyMap=Map.empty;}

    let testCauseSpec3Id = Guid.Parse "00000000-0000-0000-0000-000000000003"
    let testCauseSpec3 = { CauseSpec.id = testCauseSpec3Id; 
                           genus=["Test"; "3"]; prams=Map.empty; keyMap=Map.empty;}

module CauseTest = 
    let testOp (causeSpec:CauseSpec) =
        {Cause.causeSpec=causeSpec; op=fun (e:Enviro) -> e|>Ok}

    let fromCauseSpec (genus:string list) = 
        match genus with
        | [] -> "No CauseTest genus" |> Error
        | ["1"] -> testOp CauseSpec.testCauseSpec1 |> Ok
        | ["2"] -> testOp CauseSpec.testCauseSpec2 |> Ok
        | ["3"] -> testOp CauseSpec.testCauseSpec3 |> Ok
        | a::b -> sprintf "CauseTest: %s not handled" a |> Error


module CauseRandGen = 
    let intArrayCauseSpecId = Guid.Parse "00000000-0000-0000-0000-000000000001"
    let intArray (causeSpec:CauseSpec) = 
        let causer = fun (e:Enviro) ->
            result {
                let count = causeSpec.prams.["count"] |> int
                let! rngGen = causeSpec.prams.["rngGen"] |> RngGenDto.fromJson
                let! intDistType = causeSpec.prams.["intDistType"] |> IntDistTypeDto.fromJson
                let outName = causeSpec.prams.["outName"]
                let intDist = IntDist.makeRandom intDistType (rngGen |> Rando.fromRngGen) count
                let dto = intDist |> IntDistDto.toDto
                return! Enviro.addKvpToEnviro outName dto e
            }
        {Cause.causeSpec=causeSpec; op=causer} |> Ok
        
    let lattice2dArrayCauseSpecId = Guid.Parse "00000000-0000-0000-0000-000000000002"
    let lattice2dArray (causeSpec:CauseSpec) = 
        let causer = fun (e:Enviro) ->
            result {
                let count = causeSpec.prams.["count"] |> int
                let! rngGen = causeSpec.prams.["rngGen"] |> RngGenDto.fromJson
                let! l2dDistType = causeSpec.prams.["lattice2dDistType"] |> Lattice2dDistTypeDto.fromJson
                let outName = causeSpec.prams.["outName"]
                let l2dDist = Lattice2dDist.makeRandom l2dDistType (rngGen |> Rando.fromRngGen) count
                let dto = l2dDist |> Lattice2dDistDto.toDto
                return! Enviro.addKvpToEnviro outName dto e
            }
        {Cause.causeSpec=causeSpec; op=causer} |> Ok

    let fromCauseSpec (genus:string list) (causeSpec:CauseSpec) = 
        match genus with
        | [] -> "No CauseRandGen genus" |> Error
        | ["IntArray"] -> intArray causeSpec
        | ["LatticeLoc2dArray"] -> lattice2dArray causeSpec
        | a::b -> sprintf "CauseRandGen: %s not handled" a |> Error


module Causes =
    let noOp =
        {Cause.causeSpec=CauseSpec.noOpCauseSpec; op=fun (e:Enviro) -> e|>Ok}

    let fromCauseSpec (causeSpec:CauseSpec) = 
     match causeSpec.genus with
     | [] -> "No CauseSpec genus" |> Error
     | ["NoOp"] -> noOp |> Ok
     | "Test"::b -> (CauseTest.fromCauseSpec b)
     | "RandGen"::b -> CauseRandGen.fromCauseSpec b causeSpec
     | a::b -> sprintf "Causes: %s not handled" a |> Error
     