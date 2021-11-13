namespace global
open System

module RunBatch =

    let runBatchSeq 
            (rndCauseSpecGen : FileDir -> RandomSeed -> 
                seq<string*FileDir*causeSpec>)
            (outputDir:FileDir) 
            (seed:RandomSeed) 
            (firstDex:int)  =

        let runBatch (causeSpecDescr, outputDir, causeSpec) =
            Console.WriteLine(string causeSpecDescr)
            let res = Runs.runCauseSpec outputDir causeSpec
            match res with
            | Ok b -> b |> ignore
            | Error m -> Console.WriteLine m

        rndCauseSpecGen outputDir seed 
            |> Seq.skip firstDex
            |> Seq.iter(runBatch)

    
    let runPerfBinBatchSeq 
                    (outputDir:FileDir) 
                    (seed:RandomSeed) 
                    (firstDex:int) =

        runBatchSeq SorterPbCauseSpecGen.makeRunBatchSeq 
                    outputDir seed firstDex

    let runShcSets
                    (outputDir:FileDir) 
                    (seed:RandomSeed) 
                    (firstDex:int) =

        runBatchSeq SorterShcCauseSpecGen.makeRunBatchSeq 
                       outputDir 
                       seed
                       firstDex



  module PerfBinReports =

    let dirPerfBinBySorterGenReport (outputDir:FileDir) 
                                    (reportDir:FileDir) =

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
                let! worldMerge = worldDto |> WorldDto.fromDto
                let! map = worldMerge.cause.causeSpec.prams |> StringMapDto.fromDto
                let! sorterPerfBinsDto =  
                        Enviro.getDto<sorterPerfBinDto[]> 
                                        worldMerge.enviro
                                        binResultsName
                let! sorterRndGen = 
                        map 
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
        sprintf "%s\\%s" (FileDir.value outputDir) fileName


  module ShcReports = 
  
     let binResultsName = "sorterShcSet"

     let shcArchsFromGuid (archDs:IDataSource) 
                          (g:Guid) =
         result {
            let! ds = archDs.GetDataSource(g)
            let! worldDto = ds |> DataStoreItem.getWorldDto
            let! worldMerge = worldDto |> WorldDto.fromDto
            let! sShcResDto =  
                    Enviro.getDto<sorterShcResultsDto> 
                                    worldMerge.enviro
                                    binResultsName

            let! sShcRes = sShcResDto |> SorterShcResultsDto.fromDto
            return sShcRes.members
        }

     let unPackShcRes (shcr:sorterShcResult) =
         (shcr.spec, shcr.archives)



     let seedSeries (haTups: seq<sorterShcSpec*sorterShcArch[]>) =
    
         let dexer (arch:sorterShcArch) =
            arch.step |> StepNumber.value
     
         let specRep (spec:sorterShcSpec) = 
           [
            spec |> SorterShcSpec.seedReport;
            spec |> SorterShcSpec.mutReport;
            spec |> SorterShcSpec.sorterReport;
           ]

         (ReportUtils.partialyPadSeries haTups dexer specRep, ["seed"; "mut"; "sorter"; "temp"])



     let singleShcPivotTable (outputDir:FileDir) 
                             (reportDir:FileDir) =

          let attrLabels = [ "Energy"; "Stages"; "Switches";]
          let _attrF (sArch:sorterShcArch option) = 
              match sArch with
              | Some arch ->
                    [
                        sprintf "%.5f" (arch.energy |> Energy.value);
                        sprintf "%d" (arch.perf.usedStageCount |> StageCount.value);
                        sprintf "%d" (arch.perf.usedSwitchCount |> SwitchCount.value);
                    ]
              | None -> [""; ""; ""]
          

          let _catSeriesRept ((cats:string list),
                             (ts:sorterShcArch option[])) = 
               ts |> Array.mapi (fun dex arch -> 
                        seq {yield! cats; yield (dex|>string); yield! (_attrF arch)}
                        |> StringUtils.printSeqToRow)


          let outFileName = sprintf "%s.txt"  (System.DateTime.Now.Ticks |> string)
          let reportDataSource = new DirectoryDataSource(outputDir) 
                                      :> IDataSource
          //let repId = reportDataSource.GetDataSourceIds()
          //            |> Result.ExtractOrThrow
          //            |> Array.toList
          //            |> List.head

          //let members = shcArchsFromGuid reportDataSource repId
          //              |> Result.ExtractOrThrow

          //let goodOnes = members 
          //               |> Array.filter(fun r -> r.msg = "OK")
          //               |> Array.map(unPackShcRes)
          //               |> Array.toList
          //               |> Result.sequence
          //               |> Result.ExtractOrThrow

          let repIds = reportDataSource.GetDataSourceIds()
                      |> Result.ExtractOrThrow
                      |> Array.toList

          let members = repIds |> List.map(shcArchsFromGuid reportDataSource)
                               |> Result.sequence
                               |> Result.ExtractOrThrow
                               |> Array.concat
          let goodOnes = members 
                         |> Array.filter(fun r -> r.msg = "OK")
                         |> Array.map(unPackShcRes)
                         |> Array.toList


          let resMap, catLabels = goodOnes |> seedSeries

          let tblContent = resMap 
                            |> Map.toSeq
                            |> Seq.map(_catSeriesRept)
                            |> Array.concat

          let colHdrs = seq { yield! catLabels; yield "index"; yield! attrLabels; }
                       |> Seq.toArray
                       |> StringUtils.printSeqToRow

          let records = tblContent 
                        |> Array.append [|colHdrs|]

          //let header = "The detailed description part\nHere is more stuff you might be 
          // interested in\nId Gen pfx len win degree switch stage count"

          let csvFile = { csvFile.header = ""; 
                          csvFile.directory = reportDir;
                          csvFile.fileName = outFileName;
                          csvFile.records = records }

          let res = CsvFile.writeCsvFile csvFile
          let msg = match res with
                    | Ok _ -> "success"
                    | Error m -> m

          sprintf "%s: %s\\%s" msg (FileDir.value outputDir) outFileName



     let shcRunSeries (haTup: Guid*sorterShcSpec*sorterShcArch[]) =
         let (gu, spec, archies) = haTup
         let totReptSteps = spec.termSpec |> ShcTermSpec.getSteps
         let tics = StepNumber.logReporting totReptSteps |> Array.map(StepNumber.value)
         let dexer (arch:sorterShcArch) =
            arch.step |> StepNumber.value
         let specRep (spec:sorterShcSpec) = 
           [
            gu |> string;
            spec |> SorterShcSpec.seedReport;
            spec |> SorterShcSpec.mutReport;
            spec |> SorterShcSpec.tempReport;
            spec |> SorterShcSpec.sorterReport;
           ]
         let specs = spec |> specRep
         let archies = archies |> ReportUtils.fixedIndexReport dexer SorterShcArch.dflt tics 
         archies |> Array.map(fun arch -> (specs, arch))


     let getOkShcResults (outputDir:FileDir) =

        let _getOkRes (rvs:(Guid*Result<sorterShcResult array,string>) seq) = 
                let _filter (gu: Guid) (sqs: sorterShcResult seq) =
                    seq {
                            for sq in sqs do
                                if sq.msg = "OK" then yield (gu, sq)
                    }
                seq {
                        for rv in rvs do
                            match rv with
                            | (gu, Result.Ok gr) -> yield! (_filter gu gr)
                            | (gu, Result.Error msg) -> msg |> ignore
                }
        result {
            let reportDataSource = new DirectoryDataSource(outputDir) :> IDataSource
            let! repIds =  reportDataSource.GetDataSourceIds()
            return repIds |> Seq.map(fun gu -> (gu, shcArchsFromGuid reportDataSource gu))
                          |> _getOkRes
        }



     let fixedIndexSeries (outputDir:FileDir) 
                          (reportDir:FileDir) =

        let attrLabels = [ "Energy"; "Stages"; "Switches"; ]
        let specLabels = [ "fileName"; "seed"; "mut"; "temp"; "sorter"; "index"; ]
        let colHdrs = attrLabels |> List.append specLabels |> StringUtils.printSeqToRow

        let _attrF (tup:int*sorterShcArch) = 
                    let arch = tup |> snd
                    [
                        sprintf "%d" (tup |> fst);
                        sprintf "%.5f" (arch.energy |> Energy.value);
                        sprintf "%d" (arch.perf.usedStageCount |> StageCount.value);
                        sprintf "%d" (arch.perf.usedSwitchCount |> SwitchCount.value);
                    ]


        let yark = result {
            let! goodOnes = (getOkShcResults outputDir)
                               
            let y = goodOnes 
                       |> Seq.map(fun (gu, shcr) -> (gu, shcr.spec, shcr.archives))
                       |> Seq.map(shcRunSeries)
                       |> Seq.concat
                       |> Seq.map(fun tup -> (tup |> snd |> _attrF) 
                                             |> List.append (fst tup)
                                             |> StringUtils.printSeqToRow)
            return y |> Seq.toArray
        }

        let tblContent = yark |> Result.ExtractOrThrow

        let records = tblContent 
                      |> Array.append [|colHdrs|]

        //let header = "The detailed description part\nHere is more stuff you might be 
        // interested in\nId Gen pfx len win degree switch stage count"
        
        let outFileName = sprintf "%s.txt"  (System.DateTime.Now.Ticks |> string)
        let csvFile = { csvFile.header = ""; 
                        csvFile.directory = reportDir;
                        csvFile.fileName = outFileName;
                        csvFile.records = records }

        let res = CsvFile.writeCsvFile csvFile
        let msg = match res with
                  | Ok _ -> "success"
                  | Error m -> m
        
        sprintf "%s: %s\\%s" "msg" (FileDir.value outputDir) outFileName





    //let migratePerfBinReports (sourceDir:FileDir)
    //                          (destDir:FileDir) =

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













    //let dirPerfBinBySorterGenReport (outputDir:FileDir) 
    //                                (reportDir:FileDir) =

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
    //    sprintf "%s\\%s" (FileDir.value outputDir) fileName




    //let dirPerfBinBySorterGenReport2 (outputDir:FileDir) 
    //                                 (reportDir:FileDir) =

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
    //    sprintf "%s\\%s" (FileDir.value outputDir) fileName