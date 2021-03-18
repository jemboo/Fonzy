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


//|        Method |      Mean |    Error |   StdDev |
//|-------------- |----------:|---------:|---------:|
//|         NoSAG | 111.18 ms | 2.061 ms | 3.022 ms |
//|   SAGbySwitch |  15.14 ms | 0.302 ms | 0.886 ms |
//| SAGbySortable |  15.92 ms | 0.316 ms | 0.756 ms |


//|        Method          |     Mean |    Error |   StdDev |
//|----------------------- |---------:|---------:|---------:|
//|         NoSAG          | 25.94 ms | 0.515 ms | 1.254 ms |
//|   SAGbySwitch          | 16.87 ms | 0.330 ms | 0.577 ms |
//| SAGbySortable          | 15.33 ms | 0.305 ms | 0.720 ms |
//| evalSorter_AggBySwitch | 24.44 ms | 0.480 ms | 0.948 ms |

//|        Method |     Mean |    Error |   StdDev |
//|-------------- |---------:|---------:|---------:|
//|         NoSAG | 25.48 ms | 0.509 ms | 1.268 ms |
//|   SAGbySwitch | 17.15 ms | 0.342 ms | 0.617 ms |
//| SAGbySortable | 16.15 ms | 0.321 ms | 0.799 ms |


type BenchmarkSorterOps() =
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorter16 = RefSorter.createRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
    let sortableSet = SortableSetRollout.allBinary degree |> Result.ExtractOrThrow
    let sortableSetEx = SortableSet.Generated (SortableSetGenerated.allIntBits degree)
                            |> SortableSet.getSortableSetExplicit
                            |> Result.ExtractOrThrow
    //[<Benchmark>]
    //member this.NoSAG() =
    //    let ssR = SortingOps.EvalSorterOnSortableSetWithNoSAG 
    //                        sorter16 sortableSet SortingEval.SwitchUsePlan.All
    //    ssR 


    //[<Benchmark>]
    //member this.SAGbySwitch() =
    //    let ssR = SortingOps.EvalSorterOnSortableSetSAGbySwitch 
    //                        sorter16 sortableSet SortingEval.SwitchUsePlan.All
    //    ssR


    [<Benchmark>]
    member this.SAGbySortable() =
        let ssR = SortingOps.evalGroupBySortable 
                            sorter16 sortableSet Sorting.SwitchUsePlan.All
        ssR



    [<Benchmark>]
    member this.evalSorter_AggBySwitch() =
        let ssR = SortingOps.evalSorter 
                            sorter16 sortableSetEx Sorting.SwitchUsePlan.All
                            Sorting.EventGrouping.BySwitch
        ssR


type BenchmarkSorterSetOps() =
    let seed = 1234
    let iRando = Rando.fromRngGen (RngGen.createLcg seed)
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorterSetId = SorterSetId.fromGuid (Guid.NewGuid())
    let sorterLength = degree |> SorterLength.toMediocreRandomPerfLength 
                                                SwitchOrStage.Stage 
    let sorterCount = SorterCount.fromInt 50
    let makeRandomSorter() = 
            Sorter.createRandom degree sorterLength SwitchFrequency.max iRando



    let mediocreRandomSorters = List.init (SorterCount.value sorterCount)
                                  (fun _ -> makeRandomSorter())

    let mediocreSorterSet = 
                SorterSet.fromSorters 
                        sorterSetId 
                        degree 
                        mediocreRandomSorters

    let sortableSetEx = SortableSet.Generated 
                            (SortableSetGenerated.allIntBits degree)
                            |> SortableSet.getSortableSetExplicit
                            |> Result.ExtractOrThrow 


//|                            Method |       Mean |    Error |   StdDev |
//|---------------------------------- |-----------:|---------:|---------:|
//|    getSorterEff_Parallel_BySwitch |   616.0 ms | 11.82 ms | 12.65 ms |
//|      getSorterEff_Serial_BySwitch | 2,448.6 ms | 40.89 ms | 38.25 ms |
//| getSorterCoverage_Serial_BySwitch | 2,455.9 ms | 38.43 ms | 35.94 ms |
//|---------------------------------- |-----------:|---------:|---------:|
//|    getSorterEff_Parallel_BySwitch |   617.3 ms | 11.11 ms | 10.39 ms |
//|      getSorterEff_Serial_BySwitch | 3,289.6 ms | 35.27 ms | 31.26 ms |
//| getSorterCoverage_Serial_BySwitch | 3,311.1 ms | 50.06 ms | 46.83 ms |
//| getSorterEff_Parallel_NoGrouping  | 1.469 s    | 0.0331 s | 0.0970 s |

    //[<Benchmark>]
    //member this.getSorterEff_Parallel_NoGrouping() =
    //    let ssR = SortingOps.SorterSet.eval
    //                    mediocreSorterSet 
    //                    sortableSetEx 
    //                    Sorting.SwitchUsePlan.All
    //                    Sorting.EventGrouping.NoGrouping
    //                    (UseParallel.create true)
    //                    SortingEval.SortingRecords.getSorterEff
    //    ssR 

    [<Benchmark>]
    member this.getSorterEff_Parallel_BySwitch() =
        let ssR = SortingOps.SorterSet.eval
                        mediocreSorterSet 
                        sortableSetEx 
                        Sorting.SwitchUsePlan.All
                        Sorting.EventGrouping.BySwitch
                        (UseParallel.create true)
                        SortingEval.SortingRecords.getSorterEff
        ssR 

    [<Benchmark>]
    member this.getSorterEff_Serial_BySwitch() =
        let ssR = SortingOps.SorterSet.eval
                        mediocreSorterSet 
                        sortableSetEx 
                        Sorting.SwitchUsePlan.All
                        Sorting.EventGrouping.BySwitch
                        (UseParallel.create false)
                        SortingEval.SortingRecords.getSorterEff
        ssR 

    [<Benchmark>]
    member this.getSorterCoverage_Serial_BySwitch() =
        let ssR = SortingOps.SorterSet.eval
                        mediocreSorterSet 
                        sortableSetEx 
                        Sorting.SwitchUsePlan.All
                        Sorting.EventGrouping.BySwitch
                        (UseParallel.create false)
                        SortingEval.SortingRecords.getSorterCoverage
        ssR 