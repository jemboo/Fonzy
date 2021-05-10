// Learn more about F# at http://fsharp.org
open System
open BenchmarkDotNet.Running


[<EntryPoint>]
let main argv =
    /// ******* Benchmark ***********
    //Console.WriteLine("Starting Benchmark.main")
    //let summary = BenchmarkRunner.Run<BenchmarkSorterSetOps>()
    //printfn "%A" summary
    //Console.Read() |> ignore
   
     /// ******* Perfbins ***********
    Console.WriteLine("Starting Perfbins.main")
    let res = Array.init 10000 
                (fun i -> 
                        Console.WriteLine(i)
                        RunW.genToSorterPerfBins i)
    Console.Read() |> ignore   

    /// ******* Perfbins ***********
    //Console.WriteLine("Starting Perfbins.main")
    //let res = [|6; 7; 8; 6; 7; 8; 6; 7; 8; 6; 7; 8; 6; 7; 8; 6; 7; 8; 6; 7; 8;|]                        /////////(Array.init 1000 (id))
    //          |> Array.map
    //           (fun i -> 
    //                   Console.WriteLine(i)
    //                   RunW.genToSorterPerfBins i)
    //Console.Read() |> ignore  


     /// ******* PerfBinsReport ***********
    //Console.WriteLine("Starting PerfBinsReport.main")
    //let res2 =  RunW.dirPerfBinBySorterGenReport(33)
    //Console.Read() |> ignore

    0 // return an integer exit code
