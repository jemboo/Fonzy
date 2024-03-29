﻿namespace global
open System


type cause = { causeSpec:causeSpec; 
               op:(obj->Result<obj->unit, string>) -> enviro-> Result<enviro, string>
             }

module CauseRandGen = 
    let intArray (causeSpec:causeSpec) = 
        let causer = fun (context:obj) (e:enviro) ->
            result {
                let! map = causeSpec.prams |> StringMapDto.fromDto; 
                let! count = map |> ResultMap.lookupKeyedInt "count"
                let! rngGen = map |> ResultMap.procKeyedString "rngGen" 
                                              (RngGenDto.fromJson)
                let! intDistType = map |> ResultMap.procKeyedString "intDistType" 
                                                      (IntDistTypeDto.fromJson)

                let! outName = ResultMap.read "outName" map

                let intDist = IntDist.makeRandom intDistType 
                                                 (rngGen |> Rando.fromRngGen) 
                                                 count

                let intDistDto = intDist |> IntDistDto.toDto
                return! Enviro.addDto<intDistDto>
                                            outName 
                                            intDistDto
                                            e
            }
        {cause.causeSpec=causeSpec; op=causer} |> Ok
        

    let lattice2dArray (causeSpec:causeSpec) = 
        let causer = fun (context:obj) (e:enviro) ->
            result {
                let! map = causeSpec.prams |> StringMapDto.fromDto; 
                let count = map.["count"] |> int
                let! rngGen = map.["rngGen"] |> RngGenDto.fromJson
                let! l2dDistType = map.["lattice2dDistType"] |> Int2dDistTypeDto.fromJson

                let outName = map.["outName"]
                let l2dDist = Int2dDist.makeRandom 
                                l2dDistType 
                                (rngGen |> Rando.fromRngGen) 
                                count
                let int2dDistDto = l2dDist |> Int2dDistDto.toDto
                return! Enviro.addDto<int2dDistDto>
                                    outName 
                                    int2dDistDto
                                    e 
            }
        {cause.causeSpec=causeSpec; op=causer} |> Ok


    let fromCauseSpec (genus:string list)
                      (causeSpec:causeSpec) = 
        match genus with
        | [] -> "No CauseRandGen genus" |> Error
        | ["IntArray"] -> intArray causeSpec
        | ["Int2dArray"] -> lattice2dArray causeSpec
        | a::b -> sprintf "CauseRandGen: %s not handled" a |> Error


     