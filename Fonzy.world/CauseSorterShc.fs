namespace global
open System

module CauseSorterShc =

    let sorterShcSpecRndGen (monitor:'a->unit)
                            (causeSpec:causeSpec) =
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

        {Cause.causeSpec=causeSpec; op=causer}


    //let sorterShcSpecRndGen2 (logger:FileFolder->FileName->FileExt->seq<string>->Result<bool,string>)
    //                         (causeSpec:causeSpec) =

    let sorterShcSpecRndGen2 (monitor:'a->unit)
                             (causeSpec:causeSpec) =
        let causer = fun (e:enviro) ->
            result {
                let logShc () = 
                    fun (state:sHCstate) shc ->
                        match state with
                        | sHCstate.PostMutate -> true |> ignore
                        | sHCstate.PostEvaluate -> true |> ignore
                        | sHCstate.PostAnnealer -> true |> ignore
                        //let res = logger shc |> Result.ExtractOrThrow
                        //res |> ignore

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


                let batchRes = SHCset2.runBatch (UseParallel.create useParallel)
                                                (logShc ())
                                                 shcSet
                let shcRes = SorterSHCset2.getResults batchRes
                let sShcResultsDto = shcRes |> SorterShcResults2Dto.toDto
                return! Enviro.addDto<sorterShcResults2Dto>
                                                resultsName 
                                                sShcResultsDto 
                                                e
            }

        {Cause.causeSpec=causeSpec; op=causer}


    let fromCauseSpec  (genus:string list) 
                       (monitor:'a->unit)
                       (causeSpec:causeSpec) = 
        match genus with
        | [] -> "No CauseSorterShc genus" |> Error
        | [nameof sorterShcSpecRndGen] -> sorterShcSpecRndGen monitor causeSpec |> Ok
        | ["sorterShcSpecRndGen2"] -> sorterShcSpecRndGen2 monitor causeSpec |> Ok
        | a::b -> sprintf "CauseSpec: %s not handled" a |> Error


     