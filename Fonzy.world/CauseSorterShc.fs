namespace global
open System

module CauseSorterShc =

    let sorterShcSpecRndGen (causeSpec:causeSpec) =
        let causer = fun (e:enviro) ->
            result {
                let! map = causeSpec.prams |> StringMapDto.fromDto

                let! sorterShcSpecRndGen = 
                        map
                            |> ResultMap.procKeyedString "sorterShcSpecRndGen" 
                                                         (SorterShcSpecRndGenDto.fromJson)
                let! useParallel = 
                        map
                            |> ResultMap.lookupKeyedBool "useParallel"
    
                let! resultsName = 
                        map
                            |> ResultMap.procKeyedString "resultsName"
                                                         (id >> Result.Ok)
    
                let shcSet = SorterShcSpecRndGen.generate None None sorterShcSpecRndGen
                             |> Result.ExtractOrThrow
                             |> sHCset.makeSorterShcSet

                let batchRes = sHCset.runBatch (UseParallel.create useParallel) shcSet
                let shcRes = sHCset.getResults batchRes
                let sShcResultsDto = shcRes |> SorterShcResultsDto.toDto
                return! Enviro.addDto<sorterShcResultsDto>
                                                resultsName 
                                                sShcResultsDto 
                                                e 
            }

        {Cause.causeSpec=causeSpec; op=causer}
   

    let fromCauseSpec (genus:string list) 
                      (causeSpec:causeSpec) = 
        match genus with
        | [] -> "No CauseSorterShc genus" |> Error
        | [nameof sorterShcSpecRndGen] -> sorterShcSpecRndGen causeSpec |> Ok
        | a::b -> sprintf "CauseSpec: %s not handled" a |> Error

     