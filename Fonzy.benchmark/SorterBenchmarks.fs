namespace global
open BenchmarkDotNet.Attributes
open System


//|               Method |     Mean |    Error |   StdDev |
//|--------------------- |---------:|---------:|---------:|
//|      sorterWithNoSAG | 28.04 ms | 0.556 ms | 1.197 ms |
//| sorterMakeSwitchUses | 18.58 ms | 0.370 ms | 0.765 ms |
//|           evalSorter | 41.31 ms | 1.456 ms | 4.269 ms |
type BenchSorterOnInts() =
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorter16 = RefSorter.createRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
    let rollout = IntSetsRollout.allBinary degree |> Result.ExtractOrThrow
    let sortableSetBinary = SortableSetBinary.allIntBits degree
    
    [<Benchmark>]
    member this.sorterWithNoSAG() =
        let ssR = SortingInts.sorterWithNoSAG
                              sorter16 
                              rollout 
                              Sorting.SwitchUsePlan.All
        ssR 


    [<Benchmark>]
    member this.sorterMakeSwitchUses() =
        let ssR = SortingInts.sorterMakeSwitchUses 
                              sorter16 
                              rollout 
                              Sorting.SwitchUsePlan.All
        ssR


    [<Benchmark>]
    member this.evalSorter() =
        let ssR = SortingInts.evalSorterOnBinary 
                            sorter16 
                            sortableSetBinary 
                            Sorting.SwitchUsePlan.All
                            Sorting.EventGrouping.BySwitch
        ssR


        
//|                                     Method |     Mean |    Error |   StdDev |
//|------------------------------------------- |---------:|---------:|---------:|
//|    getSorterCoverage50_Parallel_NoGrouping |  7.754 s | 0.1898 s | 0.5536 s |
//| getSorterCoverage50_Parallel_GroupBySwitch |  3.279 s | 0.0579 s | 0.0483 s |
//|      getSorterCoverage50_Serial_NoGrouping | 31.213 s | 0.1030 s | 0.0913 s |
//|   getSorterCoverage50_Serial_GroupBySwitch | 14.823 s | 0.0582 s | 0.0545 s |
        
type BenchSorterSetOnInts() =
    let seed = 1234
    let iRando = Rando.fromRngGen (RngGen.createLcg seed)
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorterSetId = SorterSetId.fromGuid (Guid.NewGuid())
    let sorterCount = SorterCount.fromInt 50
    let sorterGen = SorterGen.RandStages 
                            ((StageCount.degreeTo999StageCount degree),
                             degree)
    let makeRandomSorter() = 
            SorterGen.createRandom sorterGen iRando

    let mediocreRandomSorters = List.init (SorterCount.value sorterCount)
                                  (fun _ -> makeRandomSorter())

    let mediocreSorterSet = 
                SorterSet.fromSorters 
                        sorterSetId
                        degree
                        mediocreRandomSorters

    let sortableSetAllBits = SortableSetSpec.Generated 
                                (SortableSetGenerated.allIntBits degree)
                             |> SortableSetSpec.getSortableSetExplicit
                             |> Result.ExtractOrThrow


    [<Benchmark>]
    member this.getSorterCoverage50_Parallel_NoGrouping() =
        let ssR = SortingOps.SorterSet.eval
                        mediocreSorterSet 
                        sortableSetAllBits
                        Sorting.SwitchUsePlan.All
                        Sorting.EventGrouping.NoGrouping
                        (UseParallel.create true)
                        (SortingEval.SortingRecords.getSorterCoverage true)
        ssR 




    [<Benchmark>]
    member this.getSorterCoverage50_Parallel_GroupBySwitch() =
        let ssR = SortingOps.SorterSet.eval
                        mediocreSorterSet 
                        sortableSetAllBits 
                        Sorting.SwitchUsePlan.All
                        Sorting.EventGrouping.BySwitch
                        (UseParallel.create true)
                        (SortingEval.SortingRecords.getSorterCoverage true)
        ssR 

    

    [<Benchmark>]
    member this.getSorterCoverage50_Serial_NoGrouping() =
        let ssR = SortingOps.SorterSet.eval
                        mediocreSorterSet 
                        sortableSetAllBits
                        Sorting.SwitchUsePlan.All
                        Sorting.EventGrouping.NoGrouping
                        (UseParallel.create false)
                        (SortingEval.SortingRecords.getSorterCoverage true)
        ssR 


    [<Benchmark>]
    member this.getSorterCoverage50_Serial_GroupBySwitch() =
        let ssR = SortingOps.SorterSet.eval
                        mediocreSorterSet 
                        sortableSetAllBits 
                        Sorting.SwitchUsePlan.All
                        Sorting.EventGrouping.BySwitch
                        (UseParallel.create false)
                        (SortingEval.SortingRecords.getSorterCoverage true)
        ssR 



//|               Method |     Mean |    Error |   StdDev |
//|--------------------- |---------:|---------:|---------:|
//|      sorterWithNoSAG | 693.3 us | 22.33 us | 65.85 us |
//| sorterMakeSwitchUses | 257.6 us |  4.73 us |  4.42 us |
//|           evalSorter | 609.5 us |  8.01 us |  7.49 us |

type BenchmarkSorterOnBp64() =
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorter16 = RefSorter.createRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
    let sortableSetbp64 = SortableSetBp64.allBp64 degree

    let sorter16 = RefSorter.createRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
    let rollout = BP64SetsRollout.allBinary degree |> Result.ExtractOrThrow
    
    //[<Benchmark>]
    //member this.sorterWithNoSAG() =
    //    let ssR = SortingBp64.sorterWithNoSAG
    //                          sorter16 
    //                          rollout 
    //                          Sorting.SwitchUsePlan.All
    //    ssR 


    [<Benchmark>]
    member this.sorterMakeSwitchUses() =
        let ssR = SortingBp64.sorterMakeSwitchUses 
                              sorter16 
                              rollout 
                              Sorting.SwitchUsePlan.All
        ssR


    [<Benchmark>]
    member this.evalSorter() =
        let ssR = SortingBp64.evalSorter 
                            sorter16 
                            sortableSetbp64 
                            Sorting.SwitchUsePlan.All
                            Sorting.EventGrouping.BySwitch
        ssR



        
//|                                     Method |       Mean |     Error |    StdDev |
//|------------------------------------------- |-----------:|----------:|----------:|
//|    getSorterCoverage50_Parallel_NoGrouping | 3,204.1 ms | 104.74 ms | 308.81 ms |
//| getSorterCoverage50_Parallel_GroupBySwitch |   255.9 ms |   3.90 ms |   3.65 ms |
//|      getSorterCoverage50_Serial_NoGrouping | 7,403.9 ms |  52.55 ms |  43.89 ms |
//|   getSorterCoverage50_Serial_GroupBySwitch | 1,043.5 ms |  12.37 ms |  10.96 ms |

type BenchmarkSorterSetOnBp64() =
    let seed = 1234
    let iRando = Rando.fromRngGen (RngGen.createLcg seed)
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorterSetId = SorterSetId.fromGuid (Guid.NewGuid())
    let sorterCount = SorterCount.fromInt 50
    let sorterGen = SorterGen.RandStages 
                            ((StageCount.degreeTo999StageCount degree),
                             degree)
    let makeRandomSorter() = 
            SorterGen.createRandom sorterGen iRando

    let mediocreRandomSorters = List.init (SorterCount.value sorterCount)
                                  (fun _ -> makeRandomSorter())

    let mediocreSorterSet = 
                SorterSet.fromSorters 
                        sorterSetId
                        degree
                        mediocreRandomSorters

    let sortableSetAllBits = SortableSetSpec.Generated 
                               (SortableSetGenerated.allBp64 degree)
                             |> SortableSetSpec.getSortableSetExplicit
                             |> Result.ExtractOrThrow 

                            
    //[<Benchmark>]
    //member this.getSorterCoverage50_Parallel_NoGrouping() =
    //    let ssR = SortingOps.SorterSet.eval
    //                    mediocreSorterSet 
    //                    sortableSetAllBits
    //                    Sorting.SwitchUsePlan.All
    //                    Sorting.EventGrouping.NoGrouping
    //                    (UseParallel.create true)
    //                    (SortingEval.SortingRecords.getSorterCoverage true)
    //    ssR 




    [<Benchmark>]
    member this.getSorterCoverage50_Parallel_GroupBySwitch() =
        let ssR = SortingOps.SorterSet.eval
                        mediocreSorterSet 
                        sortableSetAllBits 
                        Sorting.SwitchUsePlan.All
                        Sorting.EventGrouping.BySwitch
                        (UseParallel.create true)
                        (SortingEval.SortingRecords.getSorterCoverage true)
        ssR 



    //[<Benchmark>]
    //member this.getSorterCoverage50_Serial_NoGrouping() =
    //    let ssR = SortingOps.SorterSet.eval
    //                    mediocreSorterSet 
    //                    sortableSetAllBits
    //                    Sorting.SwitchUsePlan.All
    //                    Sorting.EventGrouping.NoGrouping
    //                    (UseParallel.create false)
    //                    (SortingEval.SortingRecords.getSorterCoverage true)
    //    ssR 


    //[<Benchmark>]
    //member this.getSorterCoverage50_Serial_GroupBySwitch() =
    //    let ssR = SortingOps.SorterSet.eval
    //                    mediocreSorterSet 
    //                    sortableSetAllBits 
    //                    Sorting.SwitchUsePlan.All
    //                    Sorting.EventGrouping.BySwitch
    //                    (UseParallel.create false)
    //                    (SortingEval.SortingRecords.getSorterCoverage true)
    //    ssR 