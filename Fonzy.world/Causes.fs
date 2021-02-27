namespace global
open System


type Cause = {causeSpec:CauseSpec; op:Enviro -> Result<Enviro, string>}

module CauseSorters = 
    let rndGen (causeSpec:CauseSpec) =
        let causer = fun (e:Enviro) ->
            result {
                let! degree = causeSpec.prams |> ResultMap.procKeyedInt "degree" 
                                                         (fun d -> Degree.create "" d)
                let! sorterLength = causeSpec.prams |> ResultMap.procKeyedJson "sorterLength" 
                                                          (SorterLengthDto.fromJson)
                let! switchFreq = causeSpec.prams |> ResultMap.procKeyedFloat "switchFreq" 
                                                          (fun d -> SwitchFrequency.create "" d)
                let! sorterCount = causeSpec.prams |> ResultMap.procKeyedInt "sorterCount" 
                                                          (fun d -> SorterCount.create "" d)
                let! rngGen = causeSpec.prams |> ResultMap.procKeyedJson "rngGen" 
                                                          (RngGenDto.fromJson)
                let! outName = ResultMap.read "outName" causeSpec.prams

                let randy = Rando.fromRngGen rngGen
                let sorterArray = Sorter.createRandomArray degree sorterLength
                                        switchFreq sorterCount randy

                let cereal = sorterArray |> Array.map(fun s -> s |> SorterDto.toJson)
                                         |> Json.serialize
                return! Enviro.addKvpToEnviro outName cereal e
            }
        {Cause.causeSpec=causeSpec; op=causer}


    let fromCauseSpec (genus:string list) (causeSpec:CauseSpec) = 
        match genus with
        | [] -> "No CauseSorters genus" |> Error
        | ["rndGen"] -> rndGen causeSpec |> Ok
        | a::b -> sprintf "CauseTest: %s not handled" a |> Error


module CauseRandGen = 
    let intArray (causeSpec:CauseSpec) = 
        let causer = fun (e:Enviro) ->
            result {
                let! count = causeSpec.prams |> ResultMap.lookupKeyedInt "count"
                let! rngGen = causeSpec.prams |> ResultMap.procKeyedJson "rngGen" 
                                                          (RngGenDto.fromJson)
                let! intDistType = causeSpec.prams |> ResultMap.procKeyedJson "intDistType" 
                                                      (IntDistTypeDto.fromJson)
                let! outName = ResultMap.read "outName" causeSpec.prams

                let intDist = IntDist.makeRandom intDistType 
                                                 (rngGen |> Rando.fromRngGen) 
                                                 count
                let dto = intDist |> IntDistDto.toDto
                return! Enviro.addKvpToEnviro outName dto e
            }
        {Cause.causeSpec=causeSpec; op=causer} |> Ok
        
    let lattice2dArray (causeSpec:CauseSpec) = 
        let causer = fun (e:Enviro) ->
            result {
                let count = causeSpec.prams.["count"] |> int
                let! rngGen = causeSpec.prams.["rngGen"] |> RngGenDto.fromJson
                let! l2dDistType = causeSpec.prams.["lattice2dDistType"] |> Int2dDistTypeDto.fromJson
                let outName = causeSpec.prams.["outName"]
                let l2dDist = Int2dDist.makeRandom l2dDistType (rngGen |> Rando.fromRngGen) count
                let dto = l2dDist |> Int2dDistDto.toDto
                return! Enviro.addKvpToEnviro outName dto e
            }
        {Cause.causeSpec=causeSpec; op=causer} |> Ok

    let fromCauseSpec (genus:string list) (causeSpec:CauseSpec) = 
        match genus with
        | [] -> "No CauseRandGen genus" |> Error
        | ["IntArray"] -> intArray causeSpec
        | ["Int2dArray"] -> lattice2dArray causeSpec
        | a::b -> sprintf "CauseRandGen: %s not handled" a |> Error


module Causes =
    let noOp =
        {Cause.causeSpec=CauseSpec.noOpCauseSpec; op=fun (e:Enviro) -> e|>Ok}

    let fromCauseSpec (causeSpec:CauseSpec) = 
     match causeSpec.genus with
     | [] -> "No CauseSpec genus" |> Error
     | ["NoOp"] -> noOp |> Ok
     | "Sorters"::b -> CauseSorters.fromCauseSpec b causeSpec
     | "RandGen"::b -> CauseRandGen.fromCauseSpec b causeSpec
     | a::b -> sprintf "Causes: %s not handled" a |> Error
     