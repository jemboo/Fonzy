// Learn more about F# at http://fsharp.org
open System
open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =
    let summary = BenchmarkRunner.Run<BenchmarkSorterOps>()
    printfn "%A" summary
    0 // return an integer exit code
