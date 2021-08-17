namespace global
open System

module RunBatch =

    let runBatchSeq 
            (rndCauseSpecGen : RandomSeed -> FilePath -> 
                seq<string*FilePath*CauseSpec>)
            (outputDir:FilePath) 
            (seed:RandomSeed) 
            (firstDex:int)  =

        let runBatch (causeSpecDescr, outputDir, causeSpec) =
            Console.WriteLine(string causeSpecDescr)
            let res = Runs.runCauseSpec outputDir causeSpec
            match res with
            | Ok b -> b |> ignore
            | Error m -> Console.WriteLine m

        rndCauseSpecGen seed outputDir
            |> Seq.skip firstDex
            |> Seq.iter(runBatch)

    
    let runPerfBinBatchSeq 
                    (outputDir:FilePath) 
                    (seed:RandomSeed) 
                    (firstDex:int) =

        runBatchSeq SorterPbCauseSpecGen.makeRunBatchSeq 
                    outputDir seed firstDex



  module PerfBinReports =


    let dirPerfBinBySorterGenReport (outputDir:FilePath) 
                                     (reportDir:FilePath) =

        let binResultsName = "sorterPerfBins"
        let reportDataSource = new DirectoryDataSource(outputDir) 
                                    :> IDataSource
        let repNs = reportDataSource.GetDataSourceIds()
                    |> Result.ExtractOrThrow
                    |> Array.toList

        let perfBinsFromGuid (g:Guid) =
            result {
                let! ds = reportDataSource.GetDataSource(g)
                let! worldDto = ds |> DataStoreItem.getWorldDto
                let! world = worldDto |> WorldDto.fromDto
                let! sorterPerfBinsDto =  
                        Enviro.getDto<sorterPerfBinDto[]> 
                                        world.enviro
                                        binResultsName
                let! sorterRndGen = 
                        world.cause.causeSpec.prams 
                        |> ResultMap.procKeyedString "sorterRndGen" 
                                                     (SorterRndGenDto.fromJson)

                let sorterRndGenRep = sorterRndGen |> SorterRndGen.reportString g
                return (sorterRndGenRep, sorterPerfBinsDto)
            }

        let procPbInfo (pbinfo:string*sorterPerfBinDto[])  =
            let sorterGenReport, sorterPerfBinsDto = pbinfo
            result {
                let! sorterPerfBins = SorterPerfBinDto.fromDtos sorterPerfBinsDto
                let yab =  sorterPerfBins 
                            |> Array.filter (fun pb -> pb.successCount > 0)
                            |> Array.map(fun spb -> 
                            ((sorterGenReport, spb), spb.successCount))
                return yab
            }

        let formatPerfBinTotal (bt:(string*SortingEval.sorterPerfBin)*int) = 
            let sorterGenInfo, perfBin = fst bt
            let binCount = snd bt
            //let perfBin = (fst >> snd) bt
            sprintf "%s\t%d\t%d\t%d" 
                        sorterGenInfo
                        (SwitchCount.value perfBin.usedSwitchCount) 
                        (StageCount.value perfBin.usedStageCount) 
                        binCount

        let perfBinsInfo = repNs |> List.map(perfBinsFromGuid)
                                 |> Result.sequence
                                 |> Result.ExtractOrThrow

        let listofPerfBinArrays = perfBinsInfo 
                                    |> List.map(procPbInfo)
                                    |> Result.sequence
                                    |> Result.ExtractOrThrow

        let perfBinGroups = listofPerfBinArrays 
                                    |> List.reduce(fun a b -> Array.append a b)
                                    |> Array.groupBy(fst)

        let perfBinTotals = perfBinGroups 
                                    |> Array.map(fun k ->
                                            (fst k, (snd k) |> Array.sumBy(snd)))

        let rep = perfBinTotals |> Array.map(formatPerfBinTotal)
        let header = "Id Gen pfx len win degree switch stage count"
        let fileName = sprintf "%s.txt"  (System.DateTime.Now.Ticks |> string)
        let csvFile = { csvFile.header = header; 
                        csvFile.directory = reportDir;
                        csvFile.fileName = fileName;
                        csvFile.records = rep }

        let res = CsvFile.writeCsvFile csvFile |> Result.ExtractOrThrow
        sprintf "%s\\%s" (FilePath.value outputDir) fileName



    //let migratePerfBinReports (sourceDir:FilePath)
    //                          (destDir:FilePath) =

    //    let binResultsName = "sorterPerfBins"

    //    let migrateFile (srcDs:IDataSource) 
    //                    (destDs:IDataSource) 
    //                    (fileId:Guid) =
    //        result {

    //            let! ds = srcDs.GetDataSource(fileId)
    //            let! worldDto = ds |> DataStoreItem.getWorldDto
    //            let! world = worldDto |> WorldDto.fromDto

    //            let! yab = world.cause.causeSpec 
    //                      |> CauseSpecSorters.genToRndGen
    //            let wak = yab |> CauseSorters.rndGenToPerfBins

    //            let! sorterPerfBinsDto, unusedMeta =  
    //                    Enviro.getDtoAndMetaFromEnviro<sorterPerfBinDto[]> 
    //                                    world.enviro
    //                                    binResultsName

    //            let! newEnv = sorterPerfBinsDto 
    //                         |> Enviro.addDto Enviro.Empty binResultsName

    //            let newWorld = 
    //                {
    //                    World.cause = wak
    //                    World.id = world.id
    //                    World.parentId = world.parentId
    //                    World.enviro = newEnv
    //                }

    //            let dataStore = newWorld
    //                           |> WorldDto.toDto
    //                           |> DataStoreItem.WorldDto

    //            return! dataStore |> destDs.AddNewDataStoreItem   
    //        }


    //    let sourceDataSource = new DirectoryDataSource(sourceDir) 
    //                                :> IDataSource

    //    let destDataSource = new DirectoryDataSource(destDir) 
    //                                :> IDataSource

    //    let srcFileIds = sourceDataSource.GetDataSourceIds()
    //                        |> Result.ExtractOrThrow
    //                        |> Array.toList



    //    let yab = srcFileIds |> List.map(migrateFile sourceDataSource destDataSource)
    //                             |> Result.sequence
    //                             |> Result.ExtractOrThrow


    //    None













    //let dirPerfBinBySorterGenReport (outputDir:FilePath) 
    //                                (reportDir:FilePath) =

    //    let binResultsName = "sorterPerfBins"
    //    let reportDataSource = new DirectoryDataSource(outputDir) 
    //                                :> IDataSource
    //    let repNs = reportDataSource.GetDataSourceIds()
    //                |> Result.ExtractOrThrow
    //                |> Array.toList

    //    let perfBinsFromGuid (g:Guid) =
    //        result {
    //            let! ds = reportDataSource.GetDataSource(g)
    //            let! worldDto = ds |> DataStoreItem.getWorldDto
    //            let! world = worldDto |> WorldDto.fromDto
    //            let! sorterPerfBinsDto, unusedMeta =  
    //                    Enviro.getDtoAndMetaFromEnviro<sorterPerfBinDto[]> 
    //                                    world.enviro
    //                                    binResultsName
    //            let! sorterGen = 
    //                    world.cause.causeSpec.prams 
    //                    |> ResultMap.procKeyedString "sorterGen" 
    //                                                 (SorterGenDto.fromJson)

    //            let sorterGenRep = sorterGen |> SorterGen.reportString
    //            return (sorterGenRep, sorterPerfBinsDto)
    //        }

    //    let procPbInfo (pbinfo:string*sorterPerfBinDto[])  =
    //        let sorterGenReport, sorterPerfBinsDto = pbinfo
    //        result {
    //            let! sorterPerfBins = SorterPerfBinDto.fromDtos sorterPerfBinsDto
    //            return  sorterPerfBins |> Array.map(fun spb -> 
    //                        ((sorterGenReport, spb), spb.successCount))
    //        }

    //    let formatPerfBinTotal (bt:(string*SortingEval.sorterPerfBin)*int) = 
    //        let sorterGenInfo, perfBin = fst bt
    //        let binCount = snd bt
    //        //let perfBin = (fst >> snd) bt
    //        sprintf "%s\t%d\t%d\t%d" 
    //                    sorterGenInfo
    //                    (SwitchCount.value perfBin.usedSwitchCount) 
    //                    (StageCount.value perfBin.usedStageCount) 
    //                    binCount

    //    let perfBinsInfo = repNs |> List.map(perfBinsFromGuid)
    //                            |> Result.sequence
    //                            |> Result.ExtractOrThrow

    //    let listofPerfBinArrays = perfBinsInfo 
    //                                |> List.map(procPbInfo)
    //                                |> Result.sequence
    //                                |> Result.ExtractOrThrow
    //    let perfBinGroups = listofPerfBinArrays 
    //                                |> List.reduce(fun a b -> Array.append a b)
    //                                |> Array.groupBy(fst)

    //    let perfBinTotals = perfBinGroups 
    //                                |> Array.map(fun k ->
    //                                        (fst k, (snd k) |> Array.sumBy(snd)))

    //    let rep = perfBinTotals |> Array.map(formatPerfBinTotal)
    //    let header = "Gen len win degree switch stage count"
    //    let fileName = sprintf "%s.txt"  (System.DateTime.Now.Ticks |> string)
    //    let csvFile = { csvFile.header = header; 
    //                    csvFile.directory = reportDir;
    //                    csvFile.fileName = fileName;
    //                    csvFile.records = rep}

    //    let res = CsvFile.writeCsvFile csvFile |> Result.ExtractOrThrow
    //    sprintf "%s\\%s" (FilePath.value outputDir) fileName




    //let dirPerfBinBySorterGenReport2 (outputDir:FilePath) 
    //                                 (reportDir:FilePath) =

    //    let binResultsName = "sorterPerfBins"
    //    let reportDataSource = new DirectoryDataSource(outputDir) 
    //                                :> IDataSource
    //    let repNs = reportDataSource.GetDataSourceIds()
    //                |> Result.ExtractOrThrow
    //                |> Array.toList

    //    let perfBinsFromGuid (g:Guid) =
    //        result {
    //            let! ds = reportDataSource.GetDataSource(g)
    //            let! worldDto = ds |> DataStoreItem.getWorldDto
    //            let! world = worldDto |> WorldDto.fromDto
    //            let! sorterPerfBinsDto, unusedMeta =  
    //                    Enviro.getDtoAndMetaFromEnviro<sorterPerfBinDto[]> 
    //                                    world.enviro
    //                                    binResultsName
    //            let! sorterRndGen = 
    //                    world.cause.causeSpec.prams 
    //                    |> ResultMap.procKeyedString "sorterRndGen" 
    //                                                 (SorterRndGenDto.fromJson)

    //            let sorterRndGenRep = sorterRndGen |> SorterRndGen.reportString g
    //            return (sorterRndGenRep, sorterPerfBinsDto)
    //        }

    //    let procPbInfo (pbinfo:string*sorterPerfBinDto[])  =
    //        let sorterGenReport, sorterPerfBinsDto = pbinfo
    //        result {
    //            let! sorterPerfBins = SorterPerfBinDto.fromDtos sorterPerfBinsDto
    //            return  sorterPerfBins |> Array.map(fun spb -> 
    //                        ((sorterGenReport, spb), spb.successCount))
    //        }

    //    let formatPerfBinTotal (bt:(string*SortingEval.sorterPerfBin)*int) = 
    //        let sorterGenInfo, perfBin = fst bt
    //        let binCount = snd bt
    //        //let perfBin = (fst >> snd) bt
    //        sprintf "%s\t%d\t%d\t%d" 
    //                    sorterGenInfo
    //                    (SwitchCount.value perfBin.usedSwitchCount) 
    //                    (StageCount.value perfBin.usedStageCount) 
    //                    binCount

    //    let perfBinsInfo = repNs |> List.map(perfBinsFromGuid)
    //                            |> Result.sequence
    //                            |> Result.ExtractOrThrow

    //    let listofPerfBinArrays = perfBinsInfo 
    //                                |> List.map(procPbInfo)
    //                                |> Result.sequence
    //                                |> Result.ExtractOrThrow

    //    let perfBinGroups = listofPerfBinArrays 
    //                                |> List.reduce(fun a b -> Array.append a b)
    //                                |> Array.groupBy(fst)

    //    let perfBinTotals = perfBinGroups 
    //                                |> Array.map(fun k ->
    //                                        (fst k, (snd k) |> Array.sumBy(snd)))

    //    let rep = perfBinTotals |> Array.map(formatPerfBinTotal)
    //    let header = "Gen pfx len win degree switch stage count"
    //    let fileName = sprintf "%s.txt"  (System.DateTime.Now.Ticks |> string)
    //    let csvFile = { csvFile.header = header; 
    //                    csvFile.directory = reportDir;
    //                    csvFile.fileName = fileName;
    //                    csvFile.records = rep }

    //    let res = CsvFile.writeCsvFile csvFile |> Result.ExtractOrThrow
    //    sprintf "%s\\%s" (FilePath.value outputDir) fileName