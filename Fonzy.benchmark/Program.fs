// Learn more about F# at http://fsharp.org
open System
open BenchmarkDotNet.Running


[<EntryPoint>]
let main argv =

    /// ******* runPerfBinBatchSeq ***********
    //Console.WriteLine("Starting RunBp64.runPerfBinBatchSeq2")

    //RunBatch.runPerfBinBatchSeq2 (FilePath.fromString "C:\\SimOutPfxT") 
    //                             (RandomSeed.fromNow ())
    //                             0
    //                             |> ignore
    //Console.Read() |> ignore
    //0


     /// ******* PerfBinsReport ***********
    Console.WriteLine("Starting PerfBinsReport.main")
    let res2 =  PerfBinReports.dirPerfBinBySorterGenReport 
                        (FilePath.fromString "C:\\SimOutT") 
                        (FilePath.fromString "C:\\SimOutReportsT")
    Console.WriteLine(res2)
    Console.Read() |> ignore
    0


/// ******* Benchmark ***********
    //Console.WriteLine("Starting Benchmark.main")
    //let summary = BenchmarkRunner.Run<BenchmarkSorterOnBp64>()
    //printfn "%A" summary
    //Console.Read() |> ignore
    //0


/// ******* Migrate Data ***********
    //Console.WriteLine("Starting PerfBinsReport.migratePerfBinReports")
    //let res2 =  PerfBinReports.migratePerfBinReports 
    //                    (FilePath.fromString "C:\\SimOut") 
    //                    (FilePath.fromString "C:\\SimOutT")
    //Console.WriteLine(res2)
    //Console.Read() |> ignore
    //0