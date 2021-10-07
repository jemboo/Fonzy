namespace global
open System

module CauseSorters =

    let rndGen (causeSpec:causeSpec) =
        let causer = fun (e:enviro) ->
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


    let evalToSorterPerfBins (causeSpec:causeSpec) =
        let causer = fun (e:enviro) ->
          result {
                let! sorterSetName = 
                        causeSpec.prams 
                            |> ResultMap.procKeyedString "sorterSetName"
                                                         (id >> Result.Ok)
                let! switchUsePlan = 
                        causeSpec.prams 
                        |> ResultMap.procKeyedString "switchUsePlan" 
                                                      (Json.deserialize<Sorting.switchUsePlan>)
                let! sortableSetType = 
                        causeSpec.prams 
                        |> ResultMap.procKeyedString "sortableSetType" 
                                                      (SortableSetTypeDto.fromJson)
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
        {Cause.causeSpec=causeSpec; op=causer}


    let rndGenToPerfBins (causeSpec:causeSpec) =
        let causer = fun (e:enviro) ->
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
                                                         (Json.deserialize<Sorting.switchUsePlan>)
                let! sortableSetType = 
                        causeSpec.prams 
                            |> ResultMap.procKeyedString "sortableSetType" 
                                                          (SortableSetTypeDto.fromJson)
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

        {Cause.causeSpec=causeSpec; op=causer}
   

    let fromCauseSpec (genus:string list) (causeSpec:causeSpec) = 
        match genus with
        | [] -> "No CauseSorters genus" |> Error
        | ["rndGen"] -> rndGen causeSpec |> Ok
        | ["evalToSorterPerfBins"] -> evalToSorterPerfBins causeSpec |> Ok
        | ["rndGenToPerfBins"] -> rndGenToPerfBins causeSpec |> Ok
        | a::b -> sprintf "CauseSpec: %s not handled" a |> Error

     