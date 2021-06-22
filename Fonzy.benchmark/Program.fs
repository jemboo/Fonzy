// Learn more about F# at http://fsharp.org
open System
open BenchmarkDotNet.Running


[<EntryPoint>]
let main argv =

    /// ******* Benchmark ***********
    //Console.WriteLine("Starting RunBp64.runBatchSeq")

    //RunBp64.runBatchSeq ("C:\\SimOut")
    //                    (DateTime.Now.Ticks |> int |> Math.Abs) 
    //                    0 
    //                    |> ignore
    //Console.Read() |> ignore



     /// ******* PerfBinsReport ***********
    Console.WriteLine("Starting PerfBinsReport.main")
    let res2 =  PerfBinReports.dirPerfBinBySorterGenReport ("C:\\SimOut") ("C:\\SimOutReports")
    Console.WriteLine(res2)
    Console.Read() |> ignore

    0 // return an integer exit code
