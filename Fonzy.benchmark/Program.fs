// Learn more about F# at http://fsharp.org
open System
open BenchmarkDotNet.Running


[<EntryPoint>]
let main argv =
    //let summary = BenchmarkRunner.Run<BenchmarkSorterSetOps>()
    //printfn "%A" summary
   // Console.Read() |> ignore
   
     Console.WriteLine("Starting Benchmark.main")
     let res = Array.init 10000 (fun i -> 
                                        Console.WriteLine(i)
                                        RunW.genToSorterPerfBins i)
                



     //let res2 =  RunW.dirPerfBinReport(33)
    
     //Console.WriteLine(res2 |> string)
     Console.Read() |> ignore

     0 // return an integer exit code
