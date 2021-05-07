namespace global

open System

module RunW =
    let baseDataDir = "C:\\SimOut"
    let directoryDataSource = new DirectoryDataSource(baseDataDir) 
                                :> IDataSource
    FileUtils.makeDirectory baseDataDir |> Result.ExtractOrThrow |> ignore
    let seed = DateTime.Now.Ticks |> int
    let degree = Degree.fromInt 20

    let ssAllIntBits = SortableSetGenerated.allIntBits degree
                        |> SortableSet.Generated

    let rnGen = RngGen.createLcg seed
    let randy = Rando.fromRngGen rnGen
    let binResultsName = "sorterPerfBins"
    let nextRnGen() =
        RngGen.createLcg randy.NextPositiveInt

    let genMush sg sc rng sup sbset up resn =
        CauseSpecSorters.genToSorterPerfBins 
                                ("sorterGen", sg)
                                ("sorterCount", sc)
                                ("rndGen", rng)
                                ("switchUsePlan", sup)
                                ("sortableSet", sbset)
                                ("useParallel", up)
                                ("resultsName", resn)


    let genToSorterPerfBins (dex:int) =
        let stageCount = StageCount.degreeTo999StageCount degree
        let switchCount = SwitchCount.degreeTo999SwitchCount degree
        let windowSize = StageCount.fromInt dex  //(10 + (dex % 4))
       // let sorterGen = SorterGen.RandCoComp (stageCount, degree)
       // let sorterGen = SorterGen.RandSwitches (switchCount, degree)
        let sorterGen = SorterGen.RandBuddies (stageCount, windowSize, degree)
       
        //let sorterGen = match (dex % 2) with
        //                | 0 ->  SorterGen.RandStages (stageCount, degree)
        //                | _ ->  SorterGen.RandSymmetric (stageCount, degree)

        let sorterCount = SorterCount.fromInt 2000
        let causeSpec = 
                genMush
                    sorterGen
                    sorterCount
                    (nextRnGen())
                    Sorting.SwitchUsePlan.All
                    ssAllIntBits
                    true
                    binResultsName

        let cause = causeSpec
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


        
    let dirPerfBinReport (dex:int) =
        let repDataDir = "C:\\testDirForDataSourceFixture\\20_stageGen"
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
                     Enviro.getDtoAndMetaFromEnviro<SorterPerfBinsDto[]> 
                                        world.enviro
                                        binResultsName
                return sorterPerfBinsDto
            }

        let perfBinsDto = repNs |> List.map(perfBinsFromGuid)
                                |> Result.sequence
                                |> Result.ExtractOrThrow
        let perfBinsList = perfBinsDto |> List.map(SorterPerfBinsDto.fromDtos)
                                       |> Result.sequence
                                       |> Result.ExtractOrThrow
        let perfBins = perfBinsList |> List.reduce(fun a b -> Array.append a b)
                                    |> Array.groupBy(fst)
        let perfBinTotals = perfBins |> Array.map(fun k ->
                                            (fst k, (snd k) |> Array.sumBy(snd)))
        let rep = perfBinTotals |> Array.map(fun tup -> 
                            ((fst tup).usedSwitchCount,
                             (fst tup).usedStageCount,   
                             (snd tup)))
        let p = rep |> Array.iter(fun (w, t, c) -> 
                        Console.WriteLine 
                            (sprintf "%d\t%d\t%d" (SwitchCount.value w) 
                                                  (StageCount.value t) 
                                                  c))
        dex


    let dirPerfBinBySorterGenReport (dex:int) =
        let repDataDir = "C:\\SimOut" // "C:\\runArch\\SorterGen\\16\\symVsT"
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
                        Enviro.getDtoAndMetaFromEnviro<SorterPerfBinsDto[]> 
                                        world.enviro
                                        binResultsName
                let! sorterGen = 
                        world.cause.causeSpec.prams 
                        |> ResultMap.procKeyedString "sorterGen" 
                                                     (SorterGenDto.fromJson)

                let sorterGenRep = sorterGen |> SorterGen.reportString
                return (sorterGenRep, sorterPerfBinsDto)
            }

        let procPbInfo (pbinfo:string*SorterPerfBinsDto[])  =
            let sorterGenReport, sorterPerfBinsDto = pbinfo
            result {
                let! sorterPerfBins = SorterPerfBinsDto.fromDtos sorterPerfBinsDto
                return  sorterPerfBins |> Array.map(fun tup -> 
                            ((sorterGenReport, fst tup), snd tup))
            }

        let formatPerfBinTotal (bt:(string*SortingEval.SorterPerfBin)*int) = 
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
        Console.WriteLine("Gen Win degree switch stage count")
        rep |> Array.iter(Console.WriteLine)
        dex