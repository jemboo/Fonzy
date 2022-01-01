// Learn more about F# at http://fsharp.org
open System
open BenchmarkDotNet.Running


[<EntryPoint>]
let main argv =

    let textExt = FileExt.fromString ".txt"
    
    /// ************************************************
    ///      PerfBins
    /// ************************************************


    ///// ******* makeSorterPerfBins***********
    //Console.WriteLine("Starting makeSorterPerfBins")

    //Console.WriteLine(sprintf "Started: %s "
    //                          (System.DateTime.Now.ToLongTimeString()))

    ///// ******* makeSorterPerfBins***********
    //Console.WriteLine("Starting makeSorterPerfBins")
    //Console.WriteLine(sprintf "Started: %s "
    //                          (System.DateTime.Now.ToLongTimeString()))
    //let res2 =  PerfBinRep.makeSorterPerfBins 
    //                    (FileDir.fromString "C:\\SimOut5")
    //                    (RandomSeed.fromNow ())

    //Console.WriteLine(res2)

 


    ///// ******* reportSorterPerfBins***********
    //Console.WriteLine("Starting reportSorterPerfBins")


    //let sourceDir = (FileDir.fromString "C:\\SimOut_64")
    //let mergeDir = sourceDir |> FileDir.appendFolder (FileFolder.fromString "reports") 
    //                         |> Result.ExtractOrThrow
    //let res = FileUtils.makeDirectory mergeDir |> Result.ExtractOrThrow

    //let mergeFileName = FileName.fromString (Guid.NewGuid() |> string)
    //let mergePath = FilePath.fromParts mergeDir mergeFileName textExt
    //                |> Result.ExtractOrThrow
    //let res2 =  PerfBinRep.reportSorterPerfBins 
    //                    sourceDir
    //                    mergePath

    //Console.WriteLine(res2)
    


    /// ******* RunBatch.sorterPerfBins ***********
    //Console.WriteLine("Starting MakeCauseSpecs")
    //MakeCauseSpecs.sorterPerfBins   
    //                       (FileDir.fromString "C:\\SimOutShc")
    //                       (RandomSeed.fromNow ())
    //                     |> ignore


    /// ******* runPerfBinBatchSeq ***********
    //Console.WriteLine("Starting RunBp64.runPerfBinBatchSeq")

    //RunBatch.runPerfBinBatchSeq (FileDir.fromString "C:\\SimOutTst") 
    //                            (RandomSeed.fromNow ())
    //                            0
    //                             |> ignore





     /// ******* PerfBinsReport ***********
    //Console.WriteLine("Starting PerfBinsReport.main")
    //let res2 =  PerfBinReports.dirPerfBinBySorterGenReport 
    //                    (FileDir.fromString "C:\\SimOutT") 
    //                    (FileDir.fromString "C:\\SimOutReportsT")
    //Console.WriteLine(res2)



    
    
    
    /// ******* Migrate Data ***********
        //Console.WriteLine("Starting PerfBinsReport.migratePerfBinReports")
        //let res2 =  PerfBinReports.migratePerfBinReports 
        //                    (FileDir.fromString "C:\\SimOut") 
        //                    (FileDir.fromString "C:\\SimOutT")
        //Console.WriteLine(res2)



    /// ************************************************
    ///      SorterShc
    /// ************************************************

    ///// ******* ShcRep_sorterShcMergedDtoToPivotTable ***********
    //Console.WriteLine("Starting ShcRep_sorterShcMergedDtoToPivotTable")
    //Console.WriteLine(sprintf "Started: %s "
    //                          (System.DateTime.Now.ToLongTimeString()))
    //let res2 =  ShcRep.sorterShcMergedDtoToPivotTable
    //                    (Degree.fromInt 64)
    //                    (FilePath.fromString "C:\\SimOutShc\\stacked32\\report\\merged\\merged_64.txt")
    //                    (FilePath.fromString "C:\\SimOutShc\\stacked32\\report\\merged\\merged_64_rep.txt")

    //Console.WriteLine(res2)


    /////// ******* ShcReport2_mergeShc2sByGeneration***********
    //Console.WriteLine("Starting ShcReport2_mergeShc2sByGeneration")
    //Console.WriteLine(sprintf "Started: %s "
    //                          (System.DateTime.Now.ToLongTimeString()))
    //let res2 =  ShcRep.mergeShc2sByGeneration 
    //                    (FileDir.fromString "C:\\SimOutShc\\mergeTest")
    //Console.WriteLine(res2)



    /// ******* ShcRep_mergePerfBinsForSorterShc2Dto ***********
    //Console.WriteLine("Starting ShcRep_mergePerfBinsForSorterShc2Dto")
    //Console.WriteLine(sprintf "Started: %s "
    //                          (System.DateTime.Now.ToLongTimeString()))
    //let inputDir = FileDir.fromString "C:\\SimOutShc\\stacked32\\report"
    //let mergeFolder = inputDir |> FileDir.appendFolder (FileFolder.fromString "merged")
    //                           |> Result.ExtractOrThrow
    //let mergedFileName = FileName.fromString (sprintf "merged_%d" DateTime.Now.Millisecond)
    //let mergeFilePath = FilePath.fromParts mergeFolder mergedFileName textExt
    //                    |> Result.ExtractOrThrow
    //let res2 = ShcRep.mergePerfBinsForSorterShc2Dto inputDir mergeFilePath
    //Console.WriteLine(res2)



    /// ******* ShcReport2_reportSwitchWeights ***********
    //Console.WriteLine("Starting ShcReport2_reportSwitchWeights")
    //Console.WriteLine(sprintf "Started: %s "
    //                          (System.DateTime.Now.ToLongTimeString()))
    //let res2 =  Reports.reportSwitchWeights 
    //                    (FileDir.fromString "C:\\SimOutShc\\waky")
    //Console.WriteLine(res2)




    ///// ******* RunBatch.runShcSets2 ***********
    Console.WriteLine("Starting RunBatch.runShcSets2")
    RunBatch.runShcSets2   (FileDir.fromString "C:\\SimOutShc\\stacked32")
                           (RandomSeed.fromNow ())
                           0
                         |> ignore



    // ******* RunBatch.runShcSets ***********
    //Console.WriteLine("Starting RunBatch.runShcSets")

    //RunBatch.runShcSets (FileDir.fromString "C:\\SimOutShc") 
    //                            (RandomSeed.fromNow ())
    //                            0
    //                     |> ignore



///// ******* ShcReport ***********
    //Console.WriteLine("Starting ShcReport")
    //Console.WriteLine(sprintf "Started: %s "
    //                          (System.DateTime.Now.ToLongTimeString()))
    //let res2 =  ShcReports.fixedIndexSeries 
    //                    (FileDir.fromString "C:\\SimOutShc\\16w_pfx24_1M") 
    //                    (FileDir.fromString "C:\\SimOutReportsT")
    //Console.WriteLine(res2)












///// ******* Benchmark ***********
//    Console.WriteLine("Starting Benchmark.main")
//    let summary = BenchmarkRunner.Run<BenchmarkSorterSetOnBp64>()
//    printfn "%A" summary






    Console.WriteLine(sprintf "Finished: %s "
                            (System.DateTime.Now.ToLongTimeString()))
 
    Console.Read() |> ignore
    0
