namespace global
open System


type Cause = {causeSpec:causeSpec; op:enviro -> Result<enviro, string>}

module CauseRandGen = 
    let intArray (causeSpec:causeSpec) = 
        let causer = fun (e:enviro) ->
            result {
                let! count = 
                    causeSpec.prams 
                        |> ResultMap.lookupKeyedInt "count"
                let! rngGen = 
                    causeSpec.prams 
                        |> ResultMap.procKeyedString "rngGen" 
                                                          (RngGenDto.fromJson)
                let! intDistType = 
                    causeSpec.prams 
                        |> ResultMap.procKeyedString "intDistType" 
                                                      (IntDistTypeDto.fromJson)

                let! outName = ResultMap.read "outName" causeSpec.prams

                let intDist = IntDist.makeRandom intDistType 
                                                 (rngGen |> Rando.fromRngGen) 
                                                 count

                let intDistDto = intDist |> IntDistDto.toDto
                return! Enviro.addDto<intDistDto>
                                            outName 
                                            intDistDto
                                            e
            }
        {Cause.causeSpec=causeSpec; op=causer} |> Ok
        

    let lattice2dArray (causeSpec:causeSpec) = 
        let causer = fun (e:enviro) ->
            result {
                let count = 
                    causeSpec.prams.["count"] 
                        |> int
                let! rngGen = 
                    causeSpec.prams.["rngGen"] 
                        |> RngGenDto.fromJson
                let! l2dDistType = 
                    causeSpec.prams.["lattice2dDistType"] 
                        |> Int2dDistTypeDto.fromJson

                let outName = 
                    causeSpec.prams.["outName"]
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
        {Cause.causeSpec=causeSpec; op=causer} |> Ok


    let fromCauseSpec (genus:string list) (causeSpec:causeSpec) = 
        match genus with
        | [] -> "No CauseRandGen genus" |> Error
        | ["IntArray"] -> intArray causeSpec
        | ["Int2dArray"] -> lattice2dArray causeSpec
        | a::b -> sprintf "CauseRandGen: %s not handled" a |> Error


     