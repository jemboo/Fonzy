namespace global
open BenchmarkDotNet.Attributes
open System.Security.Cryptography
open System



//[<MemoryDiagnoser>]
type VecBench() =
    let N = 50000
    let lhs = [|0UL .. (N - 1 |> uint64)|]
    let rhs = [|(N - 1 |> uint64) .. (2*N - 1 |> uint64)|]
    let res = Array.zeroCreate<uint64> N

    let lhs2 = [|0UL .. (N - 1 |> uint64)|]
    let rhs2 = [|(N - 1 |> uint64) .. (2*N - 1 |> uint64)|]
    let res2 = Array.zeroCreate<uint64> N

    let lhs3 = [|0UL .. (N - 1 |> uint64)|]
    let rhs3 = [|(N - 1 |> uint64) .. (2*N - 1 |> uint64)|]
    let res3 = Array.zeroCreate<uint64> N

    let lhs4 = [|0UL .. (N - 1 |> uint64)|]
    let rhs4 = [|(N - 1 |> uint64) .. (2*N - 1 |> uint64)|]
    let res4 = Array.zeroCreate<uint64> N

    let lhs5 = [|0UL .. (N - 1 |> uint64)|]
    let rhs5 = [|(N - 1 |> uint64) .. (2*N - 1 |> uint64)|]
    let res5 = Array.zeroCreate<uint64> N

    let lhs6 = [|0UL .. (N - 1 |> uint64)|]
    let rhs6 = [|(N - 1 |> uint64) .. (2*N - 1 |> uint64)|]
    let res6 = Array.zeroCreate<uint64> N

    let lhs7 = [|0UL .. (N - 1 |> uint64)|]
    let rhs7 = [|(N - 1 |> uint64) .. (2*N - 1 |> uint64)|]
    let res7 = Array.zeroCreate<uint64> N

    let lhs8 = [|0UL .. (N - 1 |> uint64)|]
    let rhs8 = [|(N - 1 |> uint64) .. (2*N - 1 |> uint64)|]
    let res8 = Array.zeroCreate<uint64> N


    [<Benchmark(Baseline = true)>]
    member this.aOr2() =
        VecP64.aOr2 lhs rhs res
        VecP64.aOr2 lhs2 rhs2 res2
        VecP64.aOr2 lhs3 rhs3 res3
        VecP64.aOr2 lhs4 rhs4 res4
        VecP64.aOr2 lhs5 rhs5 res5
        VecP64.aOr2 lhs6 rhs6 res6
        VecP64.aOr2 lhs7 rhs7 res7
        VecP64.aOr2 lhs8 rhs8 res8

    [<Benchmark>]
    member this.aOr() =
        VecP64.aOr lhs rhs res
        VecP64.aOr lhs2 rhs2 res2
        VecP64.aOr lhs3 rhs3 res3
        VecP64.aOr lhs4 rhs4 res4
        VecP64.aOr lhs5 rhs5 res5
        VecP64.aOr lhs6 rhs6 res6
        VecP64.aOr lhs7 rhs7 res7
        VecP64.aOr lhs8 rhs8 res8

    //[<Benchmark(Baseline = true)>]
    //member this.Standard() =
    //    let res = Array.contains 9000UL [|1UL..10000UL|]
    //    res

    //[<Benchmark>]
    //member this.Vec() =
    //    let res = FastUtils.contains 9000UL [|1UL..10000UL|]
    //    res
