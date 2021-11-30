// Learn more about F# at http://fsharp.org
open System
open BenchmarkDotNet.Running


[<EntryPoint>]
let main argv =

    /// ******* RunBatch.runShcSets2 ***********
    Console.WriteLine("Starting RunBatch.runShcSets2")
    let monitor = fun _ -> ()
    RunBatch.runShcSets2   monitor
                           (FileDir.fromString "C:\\SimOutShc")
                           (RandomSeed.fromNow ())
                           0
                         |> ignore

    Console.WriteLine(sprintf "Finished: %s " 
                                 (System.DateTime.Now.ToLongTimeString()))
    Console.Read() |> ignore
    0

    // ******* RunBatch.runShcSets ***********
    //Console.WriteLine("Starting RunBatch.runShcSets")

    //RunBatch.runShcSets (FileDir.fromString "C:\\SimOutShc") 
    //                            (RandomSeed.fromNow ())
    //                            0
    //                     |> ignore

    //Console.WriteLine(sprintf "Finished: %s " 
    //                             (System.DateTime.Now.ToLongTimeString()))
    //Console.Read() |> ignore
    //0

///// ******* ShcReport ***********
    //Console.WriteLine("Starting ShcReport")
    //Console.WriteLine(sprintf "Started: %s "
    //                          (System.DateTime.Now.ToLongTimeString()))
    //let res2 =  ShcReports.fixedIndexSeries 
    //                    (FileDir.fromString "C:\\SimOutShc\\16w_pfx24_1M") 
    //                    (FileDir.fromString "C:\\SimOutReportsT")
    //Console.WriteLine(res2)
    //Console.WriteLine(sprintf "Finished: %s "
    //                          (System.DateTime.Now.ToLongTimeString()))
    //Console.Read() |> ignore
    //0




    /// ******* runPerfBinBatchSeq ***********
    //Console.WriteLine("Starting RunBp64.runPerfBinBatchSeq")

    //RunBatch.runPerfBinBatchSeq (FileDir.fromString "C:\\SimOutTst") 
    //                            (RandomSeed.fromNow ())
    //                            0
    //                             |> ignore
    //Console.Read() |> ignore
    //0


     /// ******* PerfBinsReport ***********
    //Console.WriteLine("Starting PerfBinsReport.main")
    //let res2 =  PerfBinReports.dirPerfBinBySorterGenReport 
    //                    (FileDir.fromString "C:\\SimOutT") 
    //                    (FileDir.fromString "C:\\SimOutReportsT")
    //Console.WriteLine(res2)
    //Console.Read() |> ignore
    //0


///// ******* Benchmark ***********
//    Console.WriteLine("Starting Benchmark.main")
//    let summary = BenchmarkRunner.Run<BenchmarkSorterSetOnBp64>()
//    printfn "%A" summary
//    Console.Read() |> ignore
//    0


/// ******* Migrate Data ***********
    //Console.WriteLine("Starting PerfBinsReport.migratePerfBinReports")
    //let res2 =  PerfBinReports.migratePerfBinReports 
    //                    (FileDir.fromString "C:\\SimOut") 
    //                    (FileDir.fromString "C:\\SimOutT")
    //Console.WriteLine(res2)
    //Console.Read() |> ignore
    //0