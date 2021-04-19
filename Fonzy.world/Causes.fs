namespace global
open System


type Cause = {causeSpec:CauseSpec; op:Enviro -> Result<Enviro, string>}

module CauseSorters = 
    let rndGen (causeSpec:CauseSpec) =
        let causer = fun (e:Enviro) ->
            result {
                let! sorterSetId = 
                        causeSpec.prams 
                        |> ResultMap.procKeyedGuid "sorterSetId" 
                                                   (SorterSetId.create)
                let! sorterGen = 
                        causeSpec.prams 
                        |> ResultMap.procKeyedString "sorterGen" 
                                                      (SorterGenDto.fromJson)
                let! sorterCount = 
                        causeSpec.prams 
                        |> ResultMap.procKeyedInt "sorterCount" 
                                                  (fun d -> SorterCount.create "" d)
                let! rngGen = causeSpec.prams 
                              |> ResultMap.procKeyedString "rndGen" 
                                                           (RngGenDto.fromJson)
                let! outName = 
                        ResultMap.read "rndSorterSetName" causeSpec.prams

                let randy = Rando.fromRngGen rngGen


                let sorterArray = SorterGen.createRandomArray2 
                                                sorterGen 
                                                sorterCount 
                                                randy

                let sorterSet = SorterSet.fromSorters 
                                                sorterSetId 
                                                (sorterGen |> SorterGen.getDegree) 
                                                sorterArray

                let sorterSetDto = sorterSet |> SorterSetDto.toDto
                return! Enviro.addRootDtoToEnviro<SorterSetDto>
                                                e 
                                                outName 
                                                sorterSetDto 
                                                Map.empty
            }
        {Cause.causeSpec=causeSpec; op=causer}


    let evalToSorterPerfBins (causeSpec:CauseSpec) =
        let causer = fun (e:Enviro) ->
            result {
                let! sorterSetName = 
                        causeSpec.prams 
                            |> ResultMap.procKeyedString "sorterSetName"
                                                            (id >> Result.Ok)
                let! switchUsePlan = 
                        causeSpec.prams 
                        |> ResultMap.procKeyedString "switchUsePlan" 
                                                          (Json.deserialize<Sorting.SwitchUsePlan>)
                let! sortableSet = 
                        causeSpec.prams 
                        |> ResultMap.procKeyedString "sortableSet" 
                                                          (SortableSetDto.fromJson)
                let! useParallel = 
                        causeSpec.prams 
                        |> ResultMap.lookupKeyedBool "useParallel"

                let! resultsName = 
                        causeSpec.prams 
                        |> ResultMap.procKeyedString "resultsName"
                                                            (id >> Result.Ok)
                let! sorterSetDto, unusedMeta =  
                     Enviro.getDtoAndMetaFromEnviro<SorterSetDto> 
                                        e 
                                        sorterSetName
                let! sorterSet = sorterSetDto |> SorterSetDto.fromDto
                let! sortableSetEx = sortableSet |> SortableSet.getSortableSetExplicit
                let! perfBins = SortingOps.SorterSet.getSorterPerfBins
                                  sorterSet
                                  sortableSetEx
                                  switchUsePlan
                                  (UseParallel.create useParallel)
                let perfBinsDto = perfBins |> SorterPerfBinsDto.toDtos
                return! Enviro.addRootDtoToEnviro<SorterPerfBinsDto[]>
                                    Enviro.Empty resultsName perfBinsDto Map.empty
            }
        {Cause.causeSpec=causeSpec; op=causer}


    let genToSorterPerfBins (causeSpec:CauseSpec) =
        let causer = fun (e:Enviro) ->
            result {
                let! sorterGen = 
                        causeSpec.prams 
                            |> ResultMap.procKeyedString "sorterGen" 
                                                         (SorterGenDto.fromJson)
                let! sorterCount = 
                        causeSpec.prams 
                            |> ResultMap.procKeyedInt "sorterCount" 
                                                          (fun d -> SorterCount.create "" d)
                let! rngGen = 
                        causeSpec.prams 
                            |> ResultMap.procKeyedString "rndGen" 
                                                          (RngGenDto.fromJson)
                let! switchUsePlan = 
                        causeSpec.prams 
                            |> ResultMap.procKeyedString "switchUsePlan" 
                                                          (Json.deserialize<Sorting.SwitchUsePlan>)
                let! sortableSet = 
                        causeSpec.prams 
                            |> ResultMap.procKeyedString "sortableSet" 
                                                          (SortableSetDto.fromJson)
                let! useParallel = 
                        causeSpec.prams 
                            |> ResultMap.lookupKeyedBool "useParallel"

                let! resultsName = 
                        causeSpec.prams 
                            |> ResultMap.procKeyedString "resultsName"
                                                            (id >> Result.Ok)

                let randy = Rando.fromRngGen rngGen
                let sorterArray = SorterGen.createRandomArray2 
                                                sorterGen 
                                                sorterCount 
                                                randy
                let sorterSetId = SorterSetId.fromGuid (Guid.NewGuid())
                let sorterSet = SorterSet.fromSorters 
                                            sorterSetId 
                                            (sorterGen |> SorterGen.getDegree)
                                            sorterArray

                let! sortableSetEx = sortableSet |> SortableSet.getSortableSetExplicit
                let! perfBins = SortingOps.SorterSet.getSorterPerfBins
                                  sorterSet
                                  sortableSetEx
                                  switchUsePlan
                                  (UseParallel.create useParallel)
                let perfBinsDto = perfBins |> SorterPerfBinsDto.toDtos

                return! Enviro.addRootDtoToEnviro<SorterPerfBinsDto[]>
                                    Enviro.Empty 
                                    resultsName 
                                    perfBinsDto 
                                    Map.empty
            }
        {Cause.causeSpec=causeSpec; op=causer}



    let fromCauseSpec (genus:string list) (causeSpec:CauseSpec) = 
        match genus with
        | [] -> "No CauseSorters genus" |> Error
        | ["rndGen"] -> rndGen causeSpec |> Ok
        | ["evalToSorterPerfBins"] -> evalToSorterPerfBins causeSpec |> Ok
        | ["genToSorterPerfBins"] -> genToSorterPerfBins causeSpec |> Ok
        | a::b -> sprintf "CauseTest: %s not handled" a |> Error


module CauseRandGen = 
    let intArray (causeSpec:CauseSpec) = 
        let causer = fun (e:Enviro) ->
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
                return! Enviro.addRootDtoToEnviro<IntDistDto>
                                            e 
                                            outName 
                                            intDistDto 
                                            Map.empty
            }
        {Cause.causeSpec=causeSpec; op=causer} |> Ok
        
    let lattice2dArray (causeSpec:CauseSpec) = 
        let causer = fun (e:Enviro) ->
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
                return! Enviro.addRootDtoToEnviro<Int2dDistDto>
                                    e 
                                    outName 
                                    int2dDistDto 
                                    Map.empty
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
     