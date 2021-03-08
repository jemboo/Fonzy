namespace global
open BenchmarkDotNet.Attributes
open System.Security.Cryptography
open System



//[<MemoryDiagnoser>]
type Md5VsSha256() =
    let N = 100000
    let data = Array.zeroCreate N
    let sha256 = SHA256.Create();
    let md5 = MD5.Create()

    member this.GetData =
        data

    [<Benchmark(Baseline = true)>]
    member this.Sha256() =
        sha256.ComputeHash(data)

    [<Benchmark>]
    member this.Md5() =
        md5.ComputeHash(data)

//|    Method |     Mean |    Error |   StdDev |
//|------------- |---------:|---------:|---------:|
//| SortAllFull  | 16.41 ms | 0.377 ms | 1.111 ms |
//| SortAllCheck | 21.59 ms | 0.431 ms | 0.989 ms |
type BenchmarkSorterOps() =
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorter16 = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
    let sortableSet = SortableSetRollup.allBinary degree |> Result.ExtractOrThrow

    [<Benchmark>]
    member this.SortAllFull() =
        let res = SorterEval.fullRollup sorter16 sortableSet
        res

    [<Benchmark>]
    member this.SortAllFullR() =
        let res = SorterEval.fullRollupR sorter16 sortableSet
        res

    //[<Benchmark>]
    //member this.SortAllCheck() =
    //    let res = SorterEval.checkRollup sorter16 sortableSet
    //    res