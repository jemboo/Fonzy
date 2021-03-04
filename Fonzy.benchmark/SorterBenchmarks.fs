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
//|---------- |---------:|---------:|---------:|
//| SortAllTR | 16.41 ms | 0.377 ms | 1.111 ms |
//| SortAllTB | 21.59 ms | 0.431 ms | 0.989 ms |
type BenchmarkSorterOps() =
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorter16 = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
    let sortableSet = SortableSetRollup.allBinary degree |> Result.ExtractOrThrow

    [<Benchmark>]
    member this.SortAllTR() =
        let res = SorterOps.SortAllComplete sorter16 sortableSet
        res

    [<Benchmark>]
    member this.SortAllTB() =
        let res = SorterOps.SortAllEager sorter16 sortableSet
        res