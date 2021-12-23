namespace global
open System
open System.IO

module PerfBinRep =

    let makeSorterPerfBins (outputDir:FileDir) 
                           (seed:RandomSeed) =
        let fileName = FileName.fromString (sprintf "%s_%d" "SorterSetPerf" (RandomSeed.value seed) )
        Console.WriteLine(FileName.value fileName)
        let fileExt = FileExt.fromString ".txt"
        let filePath = FilePath.fromParts outputDir fileName fileExt
                       |> Result.ExtractOrThrow
        let swUsPln = Sorting.switchUsePlan.All


        //let halfDeg = (Degree.fromInt 64)
        //let fullDeg = (Degree.fromInt 128)
        //let swPfx = []
        //let stageCt = StageCount.fromInt 1000
        //let switchCount = SwitchCount.fromInt 80000
        //let sws = StageWindowSize.fromInt 10
        //let srtrRndGens =
        //    [
        
        //     sorterRndGen.RandRflBuddies (swPfx,stageCt, sws, fullDeg);
        //     sorterRndGen.RandRfl (swPfx,stageCt, fullDeg);
        //     sorterRndGen.RandStages (swPfx,stageCt, fullDeg);
        //     sorterRndGen.RandSwitches (swPfx,switchCount, fullDeg);
        
        //    ]
        let halfDeg = (Degree.fromInt 32)
        let fullDeg = (Degree.fromInt 64)
        let swPfx = []
        let stageCt = StageCount.fromInt 500
        let switchCount = SwitchCount.fromInt 25000
        let sws = StageWindowSize.fromInt 10
        let srtrRndGens =
            [

             sorterRndGen.RandRflBuddies (swPfx,stageCt, sws, fullDeg);
             sorterRndGen.RandRfl (swPfx,stageCt, fullDeg);
             sorterRndGen.RandStages (swPfx,stageCt, fullDeg);
             sorterRndGen.RandSwitches (swPfx,switchCount, fullDeg);

            ]


        let srtrCt = SorterCount.fromInt 2000
        let prlPrc = UseParallel.create false
        let rngGen = RngGen.createLcg seed

        let srtblStTyp = sortableSetType.BinaryMerge 
                            ([halfDeg; halfDeg;], 
                            sortableSetRep.Binary fullDeg)
        let ssetMaker = SortableSetMaker.make None

        let srtblSt = SortableSet.make ssetMaker srtblStTyp 
                      |> Result.ExtractOrThrow

       

        let randy = rngGen |> Rando.fromRngGen
        let mutable rngy = rngGen
        let aRres = 
            seq { for i = 0 to 10000 do
                    rngy <- randy |> Rando.nextRngGen
                    //let switchCt = swtchCts.[i % swtchCts.Length]
                    //Console.WriteLine (sprintf "switchct: %d" (SwitchCount.value switchCt))
                   // let srtrRndGn = sorterRndGen.RandSwitches (swPfx,switchCt, fullDeg)
                    Console.WriteLine (sprintf "index: %d" i)
                    let srtrRndGn = srtrRndGens.[i % srtrRndGens.Length]
                    let srtrArray = SorterRndGen.createRandomArray
                                        srtrRndGn 
                                        srtrCt 
                                        (Rando.fromRngGen rngy)

                    let sorterSetId = SorterSetId.fromGuid (Guid.NewGuid())
                    let srtrSet = SorterSet.fromSorters 
                                                sorterSetId
                                                (srtrRndGn |> SorterRndGen.getDegree)
                                                srtrArray

                    let sorterCovs = SortingOps.SorterSet.getSorterCoverages
                                                    srtrSet
                                                    srtblSt
                                                    swUsPln
                                                    true
                                                    prlPrc
                                      |> Result.ExtractOrThrow

                    let perfBins = sorterCovs 
                                    |> SortingEval.SorterPerfBin.fromSorterCoverages

                    let srtrStPrf = {
                        sorterSetPerf.id = sorterSetId;
                        sorterSetPerf.sorterRndGen = srtrRndGn;
                        sorterSetPerf.rngGen = rngy;
                        sorterSetPerf.sorterCount = srtrCt;
                        sorterSetPerf.sortableSetType = srtblStTyp
                        sorterSetPerf.perfBins = perfBins
                    }

                    let srterSetPerfDto = srtrStPrf |> SorterSetPerfDto.toDto

                    let fds = FileDtoStream.openSorterSetPerfDto "StackedSortables" filePath
                              |> Result.ExtractOrThrow

                    fds |> FileDtoStream.append (seq {srterSetPerfDto}) |> ignore
            } |> Seq.toArray

        sprintf "done: %d" aRres.Length



    let reportSorterPerfBins (sourceDir:FileDir) 
                            (pivotPath:FilePath) =

        let _getSorterSetPerfDto  (fpath:FilePath) =
            let fileDtoStream = FileDtoStream.openSorterSetPerfDto "" fpath
                                |> Result.ExtractOrThrow
            let items = FileDtoStream.read fileDtoStream
                                |> Result.ExtractOrThrow
            items |> List.toSeq


        let _mergePerfDtos (shcDtos:sorterSetPerfDto seq) =
            let gps = shcDtos |> Seq.groupBy(fun dto ->  (dto.sorterRndGenDto))
            let merged = gps |> Seq.map(fun tup -> (snd tup) |> SorterSetMergedPerfDto.merge)
                             |> Seq.toList
                             |> Result.sequence
                             |> Result.ExtractOrThrow
            merged


        let _writeToPivotFile sw (lines:Result<string[], string>) =
            match lines with
            | Ok lnes -> lnes |> Seq.iter(fprintfn sw "%s")
            | Error m -> m |>  (fprintfn sw "%s")
        

        let genFiles = FileUtils.getFilePathsInDirectory sourceDir "*.txt"
                       |> Result.ExtractOrThrow



        let mergedDtos = genFiles |> Seq.map(_getSorterSetPerfDto)
                                  |> Seq.concat
                                  |> _mergePerfDtos
        

        use sw = new StreamWriter(pivotPath |> FilePath.value , false)
        fprintfn sw "%s" SorterSetMergedPerfDto.pivotTableHdrs

        mergedDtos  |> Seq.map(SorterSetMergedPerfDto.toReport)
                    |> Seq.iter(_writeToPivotFile sw)
        sw.Dispose()

        "finished"