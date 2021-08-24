﻿namespace global
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
                        |> ResultMap.procKeyedString "sorterRndGen" 
                                                      (SorterRndGenDto.fromJson)
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


                let sorterArray = SorterRndGen.createRandomArray 
                                                sorterGen 
                                                sorterCount 
                                                randy

                let sorterSet = SorterSet.fromSorters 
                                                sorterSetId 
                                                (sorterGen |> SorterRndGen.getDegree) 
                                                sorterArray

                let sorterSetDto = sorterSet |> SorterSetDto.toDto
                return! Enviro.addDto<sorterSetDto>
                                                outName 
                                                sorterSetDto 
                                                e 
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
                        |> ResultMap.procKeyedString "sortableSetSpec" 
                                                      (SortableSetSpecDto.fromJson)
                let! sorterSaving = 
                        causeSpec.prams 
                        |> ResultMap.procKeyedString "sorterSaving" 
                                                      (SorterSavingDto.fromJson)
                let! useParallel = 
                        causeSpec.prams 
                        |> ResultMap.lookupKeyedBool "useParallel"

                let! resultsName = 
                        causeSpec.prams 
                        |> ResultMap.procKeyedString "resultsName"
                                                      (id >> Result.Ok)

                let! sorterSetDto =  Enviro.getDto<sorterSetDto> 
                                            e 
                                            sorterSetName

                let! sorterSet = sorterSetDto |> SorterSetDto.fromDto
                let! sortableSetEx = sortableSet |> SortableSetSpec.getSortableSet
                let! sorterCovs = SortingOps.SorterSet.getSorterCoverages
                                      sorterSet
                                      sortableSetEx
                                      switchUsePlan
                                      true
                                      (UseParallel.create useParallel)

                let perfBins = sorterCovs 
                                |> SortingEval.SorterPerfBin.fromSorterCoverages

                let perfBinsDto = perfBins |> SorterPerfBinDto.toDtos

                let selectedCovs = sorterCovs
                                    |> List.toArray
                                    |> SorterSaving.chooseSorterCoverages
                                            sorterSet.degree
                                            sorterSaving
                                    |> Array.map(SorterCoverageDto.toDto)

                let! e2 = Enviro.addDto<sorterPerfBinDto[]>
                            resultsName 
                            perfBinsDto
                            Enviro.Empty 
    
                return! Enviro.addDto<sorterCoverageDto[]>
                                    (nameof selectedCovs) 
                                    selectedCovs
                                    e2 
            }
        {Cause.causeSpec=causeSpec; op=causer}


    let rndGenToPerfBins (causeSpec:CauseSpec) =
        let causer = fun (e:Enviro) ->
            result {

                let! sorterRndGen = 
                        causeSpec.prams 
                            |> ResultMap.procKeyedString "sorterRndGen" 
                                                            (SorterRndGenDto.fromJson)
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
                let! sortableSetSpec = 
                        causeSpec.prams 
                            |> ResultMap.procKeyedString "sortableSetSpec" 
                                                            (SortableSetSpecDto.fromJson)
                let! sorterSaving = 
                        causeSpec.prams 
                        |> ResultMap.procKeyedString "sorterSaving" 
                                                      (SorterSavingDto.fromJson)
                let! useParallel = 
                        causeSpec.prams 
                            |> ResultMap.lookupKeyedBool "useParallel"
    
                let! resultsName = 
                        causeSpec.prams 
                            |> ResultMap.procKeyedString "resultsName"
                                                            (id >> Result.Ok)
    
                let randy = Rando.fromRngGen rngGen
                let sorterArray = SorterRndGen.createRandomArray 
                                                sorterRndGen 
                                                sorterCount 
                                                randy

                let sorterSetId = SorterSetId.fromGuid (Guid.NewGuid())
                let sorterSet = SorterSet.fromSorters 
                                            sorterSetId
                                            (sorterRndGen |> SorterRndGen.getDegree)
                                            sorterArray
    
                let! sortableSetEx = sortableSetSpec 
                                        |> SortableSetSpec.getSortableSet

                let (sortableSetTrim, switchUses) = 
                            sortableSetEx |> SortingOps.SortableSet.reduceByPrefix 
                                                            sorterRndGen

                let! sorterCovs = SortingOps.SorterSet.getSorterCoverages
                                        sorterSet
                                        sortableSetTrim
                                        switchUsePlan
                                        true
                                        (UseParallel.create useParallel)

                let perfBins = sorterCovs 
                                |> SortingEval.SorterPerfBin.fromSorterCoverages
                let perfBinsDto = perfBins |> SorterPerfBinDto.toDtos

                let selectedCovs = sorterCovs
                                    |> List.toArray
                                    |> SorterSaving.chooseSorterCoverages
                                            sorterSet.degree
                                            sorterSaving
                                    |> Array.map(SorterCoverageDto.toDto)

                let! e2 = Enviro.addDto<sorterPerfBinDto[]>
                            resultsName 
                            perfBinsDto
                            Enviro.Empty 
    
                return! Enviro.addDto<sorterCoverageDto[]>
                                    (nameof selectedCovs)
                                    selectedCovs
                                    e2 
            }

        {Cause.causeSpec=causeSpec; op=causer}
   
    let fromCauseSpec (genus:string list) (causeSpec:CauseSpec) = 
        match genus with
        | [] -> "No CauseSorters genus" |> Error
        | ["rndGen"] -> rndGen causeSpec |> Ok
        | ["evalToSorterPerfBins"] -> evalToSorterPerfBins causeSpec |> Ok
       // | ["genToSorterPerfBins"] -> genToSorterPerfBins causeSpec |> Ok
        | ["rndGenToPerfBins"] -> rndGenToPerfBins causeSpec |> Ok
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
                return! Enviro.addDto<intDistDto>
                                            outName 
                                            intDistDto
                                            e
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
                return! Enviro.addDto<int2dDistDto>
                                    outName 
                                    int2dDistDto
                                    e 
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
     