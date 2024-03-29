﻿namespace global
open System

module CauseSorters =

    let rndGen (causeSpec:causeSpec) =
        let causer = fun (context:obj) 
                         (e:enviro) ->
            result {
                let! map = causeSpec.prams |> StringMapDto.fromDto
                let! sorterSetId = 
                        map
                        |> ResultMap.procKeyedGuid "sorterSetId" 
                                                   (SorterSetId.create)
                let! sorterGen = 
                        map 
                        |> ResultMap.procKeyedString "sorterRndGen" 
                                                      (SorterRndGenDto.fromJson)
                let! sorterCount = 
                        map 
                        |> ResultMap.procKeyedInt "sorterCount" 
                                                  (fun d -> SorterCount.create "" d)
                let! rngGen = map
                              |> ResultMap.procKeyedString "rndGen" 
                                                           (RngGenDto.fromJson)
                let! outName = 
                        ResultMap.read "rndSorterSetName" map

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
        {cause.causeSpec=causeSpec; op=causer}



    let evalToSorterPerfBins (causeSpec:causeSpec) =
        let causer = fun context (e:enviro) ->
          result {
                let! map = causeSpec.prams |> StringMapDto.fromDto
                let! sorterSetName = 
                        map 
                            |> ResultMap.procKeyedString "sorterSetName"
                                                         (id >> Result.Ok)
                let! switchUsePlan = 
                        map 
                        |> ResultMap.procKeyedString "switchUsePlan" 
                                                      (Json.deserialize<Sorting.switchUsePlan>)
                let! sortableSetType = 
                        map 
                        |> ResultMap.procKeyedString "sortableSetType" 
                                                      (SortableSetTypeDto.fromJson)
                let! sorterSaving = 
                        map
                        |> ResultMap.procKeyedString "sorterSaving" 
                                                      (SorterSavingDto.fromJson)
                let! useParallel = 
                        map 
                        |> ResultMap.lookupKeyedBool "useParallel"

                let! resultsName = 
                        map
                        |> ResultMap.procKeyedString "resultsName"
                                                      (id >> Result.Ok)

                let! sorterSetDto =  Enviro.getDto<sorterSetDto> 
                                            e 
                                            sorterSetName

                let! sorterSet = sorterSetDto |> SorterSetDto.fromDto
                let ssetMaker = SortableSetMaker.make None

                let! srtblSt = SortableSet.make ssetMaker sortableSetType 

                let! sorterCovs = SortingOps.SorterSet.getSorterCoverages
                                      sorterSet
                                      srtblSt
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
                            enviro.Empty 
    
                return! Enviro.addDto<sorterCoverageDto[]>
                                    (nameof selectedCovs) 
                                    selectedCovs
                                    e2 
          } 
        {cause.causeSpec=causeSpec; op=causer}


    let rndGenToPerfBins (causeSpec:causeSpec) =
        let causer = fun (context:obj) 
                         (e:enviro) ->
            result {
                let! map = causeSpec.prams |> StringMapDto.fromDto

                let! sorterRndGen = 
                        map 
                            |> ResultMap.procKeyedString "sorterRndGen" 
                                                         (SorterRndGenDto.fromJson)
                let! sorterCount = 
                        map 
                            |> ResultMap.procKeyedInt "sorterCount" 
                                                       (fun d -> SorterCount.create "" d)
                let! rngGen = 
                        map 
                            |> ResultMap.procKeyedString "rndGen" 
                                                          (RngGenDto.fromJson)
                let! switchUsePlan = 
                        map 
                            |> ResultMap.procKeyedString "switchUsePlan" 
                                                         (Json.deserialize<Sorting.switchUsePlan>)
                let! sortableSetType = 
                        map
                            |> ResultMap.procKeyedString "sortableSetType" 
                                                          (SortableSetTypeDto.fromJson)
                let! sorterSaving = 
                        map 
                        |> ResultMap.procKeyedString "sorterSaving" 
                                                      (SorterSavingDto.fromJson)
                let! useParallel = 
                        map
                            |> ResultMap.lookupKeyedBool "useParallel"
    
                let! resultsName = 
                        map 
                            |> ResultMap.procKeyedString "resultsName"
                                                         (id >> Result.Ok)
    
                let randy = Rando.fromRngGen rngGen
                let sorterArray = SorterRndGen.createRandomArray 
                                                sorterRndGen 
                                                sorterCount 
                                                randy

                let sorterSetId = SorterSetId.fromGuid (Guid.NewGuid())
                let sSet = SorterSet.fromSorters 
                                            sorterSetId
                                            (sorterRndGen |> SorterRndGen.getDegree)
                                            sorterArray

                let ssetMaker = SortableSetMaker.make None

                let! srtblSt = SortableSet.make ssetMaker sortableSetType 

                let! sorterCovs = SortingOps.SorterSet.getSorterCoverages
                                        sSet
                                        srtblSt
                                        switchUsePlan
                                        true
                                        (UseParallel.create useParallel)

                let perfBins = sorterCovs 
                                |> SortingEval.SorterPerfBin.fromSorterCoverages
                let perfBinsDto = perfBins |> SorterPerfBinDto.toDtos

                let selectedCovs = sorterCovs
                                    |> List.toArray
                                    |> SorterSaving.chooseSorterCoverages
                                            sSet.degree
                                            sorterSaving
                                    |> Array.map(SorterCoverageDto.toDto)

                let! e2 = Enviro.addDto<sorterPerfBinDto[]>
                            resultsName 
                            perfBinsDto
                            enviro.Empty 
    
                return! Enviro.addDto<sorterCoverageDto[]>
                                    (nameof selectedCovs)
                                    selectedCovs
                                    e2 
            }

        {cause.causeSpec=causeSpec; op=causer}
   

    let fromCauseSpec (genus:string list)
                      (causeSpec:causeSpec) = 
        match genus with
        | [] -> "No CauseSorters genus" |> Error
        | ["rndGen"] -> rndGen causeSpec |> Ok
        | ["evalToSorterPerfBins"] -> evalToSorterPerfBins causeSpec |> Ok
        | ["rndGenToPerfBins"] -> rndGenToPerfBins causeSpec |> Ok
        | a::b -> sprintf "CauseSpec: %s not handled" a |> Error

     