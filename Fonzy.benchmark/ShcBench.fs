namespace global
open BenchmarkDotNet.Attributes
open System.Security.Cryptography
open System

type BenchmarkShc() =
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorter16 = RefSorter.createRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
    let bp64Rollout = BP64SetsRollout.allBinary degree |> Result.ExtractOrThrow
    let ssrollout = bp64Rollout |> sortableSetRollout.Bp64
    let bp64s = BitsP64.arrayOfAllFor degree

    [<Benchmark>]
    member this.singleShc() =
        let ssR = SortingBp64.sorterMakeSwitchUses 
                              sorter16 
                              bp64Rollout 
                              Sorting.switchUsePlan.All
        ssR