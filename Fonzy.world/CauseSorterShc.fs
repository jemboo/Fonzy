namespace global
open System

module CauseSorterShc =

    let sorterShcSpecRndGen (causeSpec:causeSpec) =

        let causer = fun (context:obj) (e:enviro) ->
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
    
                let! shcSpecs = SorterShcSpecRndGen.generate None None sorterShcSpecRndGen
                let shcSet = shcSpecs  |> SorterSHCset.make

                let batchRes = SHCset.runBatch (UseParallel.create useParallel)
                                                shcSet

                let shcRes = SorterSHCset.getResults batchRes
                let sShcResultsDto = shcRes |> SorterShcResultsDto.toDto
                return! Enviro.addDto<sorterShcResultsDto>
                                                resultsName 
                                                sShcResultsDto 
                                                e 
            }

        { cause.causeSpec=causeSpec; op=causer }



    let sorterShcSpecRndGen2 (causeSpec:causeSpec) =
        let causer = fun (context:(obj->Result<obj->unit, string>))
                         (e:enviro) ->
            result {

                let! map = causeSpec.prams |> StringMapDto.fromDto

                let! sorterShcSpecRndGen = 
                        map
                            |> ResultMap.procKeyedString "sorterShcSpecRndGen" 
                                                         (SorterShcSpecRndGen2Dto.fromJson)
                let! useParallel = 
                        map
                            |> ResultMap.lookupKeyedBool "useParallel"

                let! resultsName = 
                        map
                            |> ResultMap.procKeyedString "resultsName"
                                                         (id >> Result.Ok)
                let! shcSpecs = SorterShcSpecRndGen2.generate None None sorterShcSpecRndGen
                let shcSet = shcSpecs  |> SorterSHCset2.make

                //let monitorCtx, monitorMaker = context :?> (FileDir*causeSpec)*(obj -> Result<(causeSpec->(obj->unit)), string>)

                let yaba = causeSpec :> obj

               // let monitorCtx, monitorMaker = context :?> obj*(obj -> Result<(obj->(obj->unit)), string>)



                let batchRes = SHCset2.runBatch (UseParallel.create useParallel)
                                                (yaba, context)
                                                shcSet
                let shcRes = SorterSHCset2.getResults batchRes
                let sShcResultsDto = shcRes |> SorterShcResults2Dto.toDto
                return! Enviro.addDto<sorterShcResults2Dto>
                                                resultsName 
                                                sShcResultsDto 
                                                e
            }

        {cause.causeSpec=causeSpec; op=causer}



    let fromCauseSpec  (genus:string list) 
                       (causeSpec:causeSpec) = 
        match genus with
        | [] -> "No CauseSorterShc genus" |> Error
        | [nameof sorterShcSpecRndGen] -> sorterShcSpecRndGen causeSpec |> Ok
        | ["sorterShcSpecRndGen2"] -> sorterShcSpecRndGen2 causeSpec |> Ok
        | a::b -> sprintf "CauseSpec: %s not handled" a |> Error


     