// Learn more about F# at http://fsharp.org
open System
open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =
    let summary = BenchmarkRunner.Run<BenchmarkSorterSetOps>()
    printfn "%A" summary
    Console.Read() |> ignore
    0 // return an integer exit code
