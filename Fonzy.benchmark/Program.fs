// Learn more about F# at http://fsharp.org
open System
open BenchmarkDotNet.Running


[<EntryPoint>]
let main argv =

    /// ******* runPerfBinBatchSeq ***********
    Console.WriteLine("Starting RunBp64.runPerfBinBatchSeq2")

    RunBatch.runPerfBinBatchSeq2 (FilePath.fromString "C:\\SimOut2") 
                                 RandomSeed.fromNow
                                0
                                |> ignore
    Console.Read() |> ignore



     /// ******* PerfBinsReport ***********
    //Console.WriteLine("Starting PerfBinsReport.main")
    //let res2 =  PerfBinReports.dirPerfBinBySorterGenReport2 
    //                    (FilePath.fromString "C:\\SimOut3") 
    //                    (FilePath.fromString "C:\\SimOutReports3")
    //Console.WriteLine(res2)
    //Console.Read() |> ignore
    0 // return an integer exit code


/// ******* Benchmark ***********
    //Console.WriteLine("Starting Benchmark.main")
    //let summary = BenchmarkRunner.Run<BenchmarkSorterOnBp64>()
    //printfn "%A" summary
    //Console.Read() |> ignore
    0 // return an integer exit code