namespace global
open System

module RunBp64 =

    let runBatchSeq (outputDir:string) 
                    (seed:int) 
                    (firstDex:int) =

        let runBatch (causeSpecDescr, outputDir, causeSpec) =
            let res = Runs.runCauseSpec causeSpecDescr outputDir causeSpec
            match res with
            | Ok b -> b |> ignore
            | Error m -> Console.WriteLine m

        SorterPerfBinGen.makeRunBatchSeq seed outputDir
            |> Seq.skip firstDex
            |> Seq.iter(runBatch)



  module PerfBinReports =

    let dirPerfBinBySorterGenReport (outputDir:string) 
                                    (reportDir:string) =

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
                let! sorterPerfBinsDto, unusedMeta =  
                        Enviro.getDtoAndMetaFromEnviro<sorterPerfBinDto[]> 
                                        world.enviro
                                        binResultsName
                let! sorterGen = 
                        world.cause.causeSpec.prams 
                        |> ResultMap.procKeyedString "sorterGen" 
                                                     (SorterGenDto.fromJson)

                let sorterGenRep = sorterGen |> SorterGen.reportString
                return (sorterGenRep, sorterPerfBinsDto)
            }

        let procPbInfo (pbinfo:string*sorterPerfBinDto[])  =
            let sorterGenReport, sorterPerfBinsDto = pbinfo
            result {
                let! sorterPerfBins = SorterPerfBinDto.fromDtos sorterPerfBinsDto
                return  sorterPerfBins |> Array.map(fun spb -> 
                            ((sorterGenReport, spb), spb.successCount))
            }

        let formatPerfBinTotal (bt:(string*SortingEval.sorterPerfBin)*int) = 
            let sorterGenInfo = (fst >> fst) bt
            let binCount = snd bt
            let perfBin = (fst >> snd) bt
            sprintf "%s\t%d\t%d\t%d" sorterGenInfo
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
        let header = "Gen win degree switch stage count"
        let fileName = sprintf "%s.txt"  (System.DateTime.Now.Ticks |> string)
        let csvFile = { csvFile.header = header; 
                        csvFile.directory = reportDir;
                        csvFile.fileName = fileName;
                        csvFile.records = rep}

        let res = CsvFile.writeCsvFile csvFile |> Result.ExtractOrThrow
        sprintf "%s\\%s" outputDir fileName