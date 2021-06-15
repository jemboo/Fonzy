namespace global

open System

module RunW =

    let nextRnGen(randy:IRando) =
        RngGen.createLcg randy.NextPositiveInt


    let baseDataDir = "C:\\SimOut"
    let directoryDataSource = new DirectoryDataSource(baseDataDir) 
                                :> IDataSource
    FileUtils.makeDirectory baseDataDir |> Result.ExtractOrThrow |> ignore


    let seed = DateTime.Now.Ticks |> int
    let degree = Degree.fromInt 16

    let ssAllIntBits = SortableSetGenerated.allIntBits degree
                        |> SortableSetSpec.Generated

    let rnGen = RngGen.createLcg seed
    let randy = Rando.fromRngGen rnGen


    let genToSorterPerfBins (dex:int) (seed:int) =
        let binResultsName = "sorterPerfBins"
    
        let srtbSetSpec = SortableSetGenerated.allBp64 degree
                            |> SortableSetSpec.Generated
        let stageCount = StageCount.degreeTo999StageCount degree
        let randy = RngGen.createLcg seed |> Rando.fromRngGen
        let switchCount = SwitchCount.degreeTo999SwitchCount degree
        let windowSize = StageCount.fromInt 3  //(10 + (dex % 4))
       // let sorterGen = SorterGen.RandCoComp (stageCount, degree)
       // let sorterGen = SorterGen.RandSwitches (switchCount, degree)
        let sorterGen = SorterGen.RandBuddies (stageCount, windowSize, degree)
       
        //let sorterGen = match (dex % 2) with
        //                | 0 ->  SorterGen.RandStages (stageCount, degree)
        //                | _ ->  SorterGen.RandSymmetric (stageCount, degree)

        let sorterCount = SorterCount.fromInt 20

        let cause = SorterPerfBinGen.makeCauseSpec
                        sorterGen
                        sorterCount
                        (nextRnGen(randy))
                        Sorting.SwitchUsePlan.All
                        srtbSetSpec
                        (UseParallel.create true)
                        binResultsName
                    |> Causes.fromCauseSpec
                    |> Result.ExtractOrThrow

        let binSpecWorld = 
            World.createFromParent 
                World.empty
                cause
            |> Result.ExtractOrThrow
            |> WorldDto.toDto
            |> DataStoreItem.WorldDto


        let fp = directoryDataSource.AddNewDataStoreItem 
                    binSpecWorld
                 |> Result.ExtractOrThrow
        dex



module RunBp64 =

    let runBatchSeq (seed:int) (firstDex:int) =

        let outputDir = "C:\\SimOut"
        let runBatch (causeSpecDescr, outputDir, causeSpec) =
            let res = Runs.runCauseSpec causeSpecDescr outputDir causeSpec
            match res with
            | Ok b -> b |> ignore
            | Error m -> Console.WriteLine m

        SorterPerfBinGen.makeRunBatchSeq seed outputDir
            |> Seq.skip firstDex
            |> Seq.iter(runBatch)


    let runBatch (degree : Degree) 
                 (seed : int) 
                 (sorterGen: SorterGen) 
                 (sorterCount : SorterCount) 
                 (useParallel : UseParallel) =

        let outputDir = "C:\\SimOut"
        let binResultsName = "sorterPerfBins"


        Console.WriteLine( sprintf "Time: %s SorterGen: %s SorterCount: %d" 
                                    (System.DateTime.Now.ToLongTimeString())
                                    (sorterGen |> SorterGen.reportString) 
                                    (SorterCount.value sorterCount))


        let srtbSetSpec = SortableSetGenerated.allBp64 degree
                            |> SortableSetSpec.Generated
        let randy = RngGen.createLcg seed |> Rando.fromRngGen
        let cause = SorterPerfBinGen.makeCauseSpec
                        sorterGen
                        sorterCount
                        (RunW.nextRnGen(randy))
                        Sorting.SwitchUsePlan.All
                        srtbSetSpec
                        (UseParallel.create true)
                        binResultsName
                    |> Causes.fromCauseSpec
                    |> Result.ExtractOrThrow

        let binSpecWorld = 
            World.createFromParent 
                World.empty
                cause
            |> Result.ExtractOrThrow
            |> WorldDto.toDto
            |> DataStoreItem.WorldDto

        let fp = RunW.directoryDataSource.AddNewDataStoreItem 
                    binSpecWorld
                 |> Result.ExtractOrThrow
        seed


  module PerfBinReports =

    let dirPerfBinReport (dex:int) =
        let binResultsName = "sorterPerfBins"
        let repDataDir =  "C:\\SimOut"
        let reportDataSource = new DirectoryDataSource(repDataDir) 
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
                return sorterPerfBinsDto
            }

        let perfBinsDto = repNs |> List.map(perfBinsFromGuid)
                                |> Result.sequence
                                |> Result.ExtractOrThrow
        let perfBinsList = perfBinsDto |> List.map(SorterPerfBinDto.fromDtos)
                                       |> Result.sequence
                                       |> Result.ExtractOrThrow
        let perfBins = perfBinsList |> List.reduce(fun a b -> Array.append a b)
                                    |> Array.groupBy(fun pb-> (pb.usedStageCount, pb.usedSwitchCount))
        let perfBinTotals = perfBins |> Array.map(fun k ->
                                            (fst k, (snd k) |> Array.sumBy(fun pb->pb.successCount)))
        let rep = perfBinTotals |> Array.map(fun tup -> 
                            (StageCount.value (tup |> fst |> fst)),
                            (SwitchCount.value (tup |> fst |> snd)),   
                             (snd tup))
        let p = rep |> Array.iter(fun (w, t, c) -> 
                        Console.WriteLine 
                            (sprintf "%d\t%d\t%d" w t c))
        dex



    let dirPerfBinBySorterGenReport (outputDir:string) (reportDir:string) =

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