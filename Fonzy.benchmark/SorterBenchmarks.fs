namespace global
open BenchmarkDotNet.Attributes
open System.Security.Cryptography
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
        let ssR = SortingInts.evalSorter 
                            sorter16 
                            sortableSetBinary 
                            Sorting.SwitchUsePlan.All
                            Sorting.EventGrouping.BySwitch
        ssR



type BenchSorterSetOnInts() =
    let seed = 1234
    let iRando = Rando.fromRngGen (RngGen.createLcg seed)
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorterSetId = SorterSetId.fromGuid (Guid.NewGuid())
    let sorterLength = degree |> SwitchOrStageCount.toMediocreRandomPerfLength 
                                                SwitchOrStage.Stage 
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

    let sortableSetEx = SortableSetSpec.Generated 
                            (SortableSetGenerated.allIntBits degree)
                            |> SortableSetSpec.getSortableSetExplicit
                            |> Result.ExtractOrThrow


//|                         Method |    Mean |    Error |   StdDev |
//|------------------------------- |--------:|---------:|---------:|
//| getSorterEff_Parallel_BySwitch | 1.643 s | 0.0326 s | 0.0305 s |



                            
//|                            Method |       Mean |    Error |   StdDev |
//|---------------------------------- |-----------:|---------:|---------:|
//|    getSorterEff_Parallel_BySwitch |   711.9 ms | 13.60 ms | 13.97 ms |
//|      getSorterEff_Serial_BySwitch | 4,104.7 ms | 68.45 ms | 64.03 ms |
//| getSorterCoverage_Serial_BySwitch | 5,652.7 ms | 47.77 ms | 44.68 ms |

//|                           Method |    Mean |    Error |   StdDev |
//|--------------------------------- |--------:|---------:|---------:|
//| getSorterEff_Parallel_NoGrouping | 2.363 s | 0.0603 s | 0.1778 s |


//|                                Method |    Mean |    Error |   StdDev |  Median |
//|-------------------------------------- |--------:|---------:|---------:|--------:|
//| getSorterPerfBins_Parallel_NoGrouping | 2.186 s | 0.0497 s | 0.1450 s | 2.122 s |

// * Warnings *
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

    
    
    //|                                Method |     Mean |    Error |   StdDev |
    //|-------------------------------------- |---------:|---------:|---------:|
    //| getSorterPerfBins_Parallel_NoGrouping | 732.6 ms | 13.69 ms | 14.06 ms |

    //[<Benchmark>]
    //member this.getSorterPerfBins_Parallel_NoGrouping() =
        //let perfBins = SortingOps.SorterSet.getSorterPerfBins
        //    mediocreSorterSet
        //    sortableSetEx
        //    Sorting.SwitchUsePlan.All
        //    (UseParallel.create true)


        //let yab  = perfBins |> Result.ExtractOrThrow
        //let ct = yab |> Array.sumBy(snd)
        ////match yab with
        ////          | SortingEval.SorterPerfBins b -> b |> Array.sumBy(snd)
        ////          | _ -> 0

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

    //[<Benchmark>]
    //member this.getSorterEff_Serial_BySwitch() =
    //    let ssR = SortingOps.SorterSet.eval
    //                    mediocreSorterSet 
    //                    sortableSetEx 
    //                    Sorting.SwitchUsePlan.All
    //                    Sorting.EventGrouping.BySwitch
    //                    (UseParallel.create false)
    //                    SortingEval.SortingRecords.getSorterEff
    //    ssR 

    //[<Benchmark>]
    //member this.getSorterCoverage_Serial_BySwitch() =
    //    let ssR = SortingOps.SorterSet.eval
    //                    mediocreSorterSet 
    //                    sortableSetEx 
    //                    Sorting.SwitchUsePlan.All
    //                    Sorting.EventGrouping.BySwitch
    //                    (UseParallel.create false)
    //                    SortingEval.SortingRecords.getSorterCoverage
    //    ssR 






//|               Method |     Mean |    Error |  StdDev |
//|--------------------- |---------:|---------:|--------:|
//|      sorterWithNoSAG |       NA |       NA |      NA |
//| sorterMakeSwitchUses | 233.5 us |  4.20 us | 3.92 us |
//|           evalSorter | 621.4 us | 10.21 us | 9.55 us |

type BenchmarkSorterOnBp64() =
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorter16 = RefSorter.createRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
    let sortableSetbp64 = SortableSetBp64.allIntBits degree

    let sorter16 = RefSorter.createRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
    let rollout = BP64SetsRollout.allBinary degree |> Result.ExtractOrThrow
    
    [<Benchmark>]
    member this.sorterWithNoSAG() =
        let ssR = SortingBp64.sorterWithNoSAG
                              sorter16 
                              rollout 
                              Sorting.SwitchUsePlan.All
        ssR 


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




type BenchmarkSorterSetOnBp64() =
    let seed = 1234
    let iRando = Rando.fromRngGen (RngGen.createLcg seed)
    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    let sorterSetId = SorterSetId.fromGuid (Guid.NewGuid())
    let sorterLength = degree |> SwitchOrStageCount.toMediocreRandomPerfLength 
                                                SwitchOrStage.Stage 
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

    let sortableSetEx = SortableSetSpec.Generated 
                            (SortableSetGenerated.allIntBits degree)
                            |> SortableSetSpec.getSortableSetExplicit
                            |> Result.ExtractOrThrow 

                            
//|                            Method |       Mean |    Error |   StdDev |
//|---------------------------------- |-----------:|---------:|---------:|
//|    getSorterEff_Parallel_BySwitch |   711.9 ms | 13.60 ms | 13.97 ms |
//|      getSorterEff_Serial_BySwitch | 4,104.7 ms | 68.45 ms | 64.03 ms |
//| getSorterCoverage_Serial_BySwitch | 5,652.7 ms | 47.77 ms | 44.68 ms |

//|                           Method |    Mean |    Error |   StdDev |
//|--------------------------------- |--------:|---------:|---------:|
//| getSorterEff_Parallel_NoGrouping | 2.363 s | 0.0603 s | 0.1778 s |


//|                                Method |    Mean |    Error |   StdDev |  Median |
//|-------------------------------------- |--------:|---------:|---------:|--------:|
//| getSorterPerfBins_Parallel_NoGrouping | 2.186 s | 0.0497 s | 0.1450 s | 2.122 s |

// * Warnings *
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

    
    
    //|                                Method |     Mean |    Error |   StdDev |
    //|-------------------------------------- |---------:|---------:|---------:|
    //| getSorterPerfBins_Parallel_NoGrouping | 732.6 ms | 13.69 ms | 14.06 ms |

    //[<Benchmark>]
    //member this.getSorterPerfBins_Parallel_NoGrouping() =
        //let perfBins = SortingOps.SorterSet.getSorterPerfBins
        //    mediocreSorterSet
        //    sortableSetEx
        //    Sorting.SwitchUsePlan.All
        //    (UseParallel.create true)


        //let yab  = perfBins |> Result.ExtractOrThrow
        //let ct = yab |> Array.sumBy(snd)
        ////match yab with
        ////          | SortingEval.SorterPerfBins b -> b |> Array.sumBy(snd)
        ////          | _ -> 0

    //[<Benchmark>]
    //member this.getSorterEff_Parallel_BySwitch() =
    //    let ssR = SortingBp32.SorterSet.eval
    //                    mediocreSorterSet 
    //                    sortableSetEx 
    //                    Sorting.SwitchUsePlan.All
    //                    Sorting.EventGrouping.BySwitch
    //                    (UseParallel.create true)
    //                    SortingEval.SortingRecords.getSorterEff
    //    ssR 

    //[<Benchmark>]
    //member this.getSorterEff_Serial_BySwitch() =
    //    let ssR = SortingOps.SorterSet.eval
    //                    mediocreSorterSet 
    //                    sortableSetEx 
    //                    Sorting.SwitchUsePlan.All
    //                    Sorting.EventGrouping.BySwitch
    //                    (UseParallel.create false)
    //                    SortingEval.SortingRecords.getSorterEff
    //    ssR 

    //[<Benchmark>]
    //member this.getSorterCoverage_Serial_BySwitch() =
    //    let ssR = SortingOps.SorterSet.eval
    //                    mediocreSorterSet 
    //                    sortableSetEx 
    //                    Sorting.SwitchUsePlan.All
    //                    Sorting.EventGrouping.BySwitch
    //                    (UseParallel.create false)
    //                    SortingEval.SortingRecords.getSorterCoverage
    //    ssR 