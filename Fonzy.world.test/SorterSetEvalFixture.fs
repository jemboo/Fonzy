namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open System.Diagnostics

[<TestClass>]
type SorterSetEvalFixture () =

    [<TestMethod>]
    member this.testBp64NoGrouping() =
        let seed = 1234 |> RandomSeed.fromInt
        let iRando = Rando.fromRngGen (RngGen.createLcg seed)
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let sorterSetId = SorterSetId.fromGuid (Guid.NewGuid())
        let sorterCount = SorterCount.fromInt 50
        let sorterRndGen = sorterRndGen.RandStages
                               ( [],
                               (StageCount.degreeTo999StageCount degree),
                               degree)
        let makeRandomSorter() = 
                SorterRndGen.createRandom sorterRndGen iRando

        let mediocreRandomSorters = List.init (SorterCount.value sorterCount)
                                      (fun _ -> makeRandomSorter())

        let mediocreSorterSet = 
                    SorterSet.fromSorters 
                            sorterSetId
                            degree
                            mediocreRandomSorters

        let sortableSetAllBits = sortableSetSpec.Generated 
                                   (SortableSetGen.allBp64 degree)
                                 |> SortableSetSpec.getSortableSet
                                 |> Result.ExtractOrThrow 

        let ssR = SortingOps.SorterSet.eval
                        mediocreSorterSet 
                        sortableSetAllBits
                        Sorting.switchUsePlan.All
                        Sorting.eventGrouping.NoGrouping
                        (UseParallel.create true)
                        (SortingEval.SorterCoverage.fromSwitchEventRecords true)
         
                    |> Result.ExtractOrThrow

        Assert.IsTrue(ssR.Length > 0)

        Assert.IsTrue(true)


    [<TestMethod>]
    member this.allMaskPerfs() =
        let degSrc = Degree.fromInt 16
        let degDest = Degree.fromInt 10
        let srtGreen = RefSorter.createRefSorter RefSorter.Degree16
                       |> Result.ExtractOrThrow
        let subSorters = Switch.allMasks degSrc degDest srtGreen.switches
                         |> Seq.map(fun sa -> Sorter.fromSwitches degDest sa)
                         |> Seq.truncate 5000
                         |> Seq.toArray

        let sorterSetId = SorterSetId.fromGuid (Guid.NewGuid())

        let subSorterSet = 
                    SorterSet.fromSorters 
                            sorterSetId
                            degDest
                            subSorters

        let sortableSetAllBits = sortableSetSpec.Generated 
                                   (SortableSetGen.allBp64 degDest)
                                 |> SortableSetSpec.getSortableSet
                                 |> Result.ExtractOrThrow 


        let ssR = SortingOps.SorterSet.eval
                        subSorterSet 
                        sortableSetAllBits
                        Sorting.switchUsePlan.All
                        Sorting.eventGrouping.NoGrouping
                        (UseParallel.create true)
                        (SortingEval.SorterCoverage.fromSwitchEventRecords true)
                    |> Result.ExtractOrThrow


        let getSorterLength (sc:SortingEval.sorterCoverage) =
            subSorterSet.sorters.[sc.sorterId].switchCount
            |> SwitchCount.value

        let report (sc:SortingEval.sorterCoverage) =
           sprintf "%d\t%s" (getSorterLength sc) 
                            (SortingEval.SorterPerf.report sc.perf)


        let winners =
            ssR |> List.filter(fun sc -> SortingEval.SorterPerf.isSucessful sc.perf)
        
        winners |> List.iter(fun sc -> Debug.WriteLine (report sc))

        Assert.IsTrue(subSorters.Length > 0)

    [<TestMethod>]
    member this.allMaskPerfs2() =
        let degSrc = Degree.fromInt 16
        let degDest = Degree.fromInt 10
        let srtGreen = RefSorter.createRefSorter RefSorter.Degree16
                       |> Result.ExtractOrThrow
        let subSorters = Switch.allMasks degSrc degDest srtGreen.switches
                         |> Seq.map(fun sa -> Sorter.fromSwitches degDest sa)
                         |> Seq.truncate 500000
                         |> Seq.toArray

        let sorterSetId = SorterSetId.fromGuid (Guid.NewGuid())

        let subSorterSet = 
                    SorterSet.fromSorters 
                            sorterSetId
                            degDest
                            subSorters

        let sortableSetAllBits = sortableSetSpec.Generated 
                                   (SortableSetGen.allBp64 degDest)
                                 |> SortableSetSpec.getSortableSet
                                 |> Result.ExtractOrThrow 


        let sorterCovs = SortingOps.SorterSet.getSorterCoverages
                              subSorterSet
                              sortableSetAllBits
                              Sorting.switchUsePlan.All
                              true
                              (UseParallel.create false)
                         |> Result.ExtractOrThrow 

        let perfBins = sorterCovs 
                        |> SortingEval.SorterPerfBin.fromSorterCoverages


        let selectedCovs = sorterCovs
                            |> List.toArray
                            |> SorterSaving.chooseSorterCoverages
                                    subSorterSet.degree
                                    sorterSaving.Successful


        let ssR = SortingOps.SorterSet.eval
                        subSorterSet 
                        sortableSetAllBits
                        Sorting.switchUsePlan.All
                        Sorting.eventGrouping.NoGrouping
                        (UseParallel.create true)
                        (SortingEval.SorterCoverage.fromSwitchEventRecords true)
                    |> Result.ExtractOrThrow


        let getSorterLength (sc:SortingEval.sorterCoverage) =
            subSorterSet.sorters.[sc.sorterId].switchCount
            |> SwitchCount.value

        let report (sc:SortingEval.sorterCoverage) =
           sprintf "%d\t%s" (getSorterLength sc) 
                            (SortingEval.SorterPerf.report sc.perf)


        let winners =
            ssR |> List.filter(fun sc -> SortingEval.SorterPerf.isSucessful sc.perf)
    
        winners |> List.iter(fun sc -> Debug.WriteLine (report sc))

        Assert.IsTrue(subSorters.Length > 0)



    [<TestMethod>]
    member this.rndMaskPerfs() =
        let seed = 1234 |> RandomSeed.fromInt
        let iRando = Rando.fromRngGen (RngGen.createLcg seed)
        let degSrc = Degree.fromInt 16
        let degDest = Degree.fromInt 10
        let srtGreen = RefSorter.createRefSorter RefSorter.Degree16
                       |> Result.ExtractOrThrow

        let subSorters = Switch.rndMasks degSrc degDest srtGreen.switches iRando
                         |> Seq.map(fun sa -> Sorter.fromSwitches degDest sa)
                         |> Seq.truncate 5000
                         |> Seq.toArray

        let sorterSetId = SorterSetId.fromGuid (Guid.NewGuid())

        let subSorterSet = 
                    SorterSet.fromSorters 
                            sorterSetId
                            degDest
                            subSorters

        let sortableSetAllBits = sortableSetSpec.Generated 
                                   (SortableSetGen.allBp64 degDest)
                                 |> SortableSetSpec.getSortableSet
                                 |> Result.ExtractOrThrow 


        let ssR = SortingOps.SorterSet.eval
                        subSorterSet 
                        sortableSetAllBits
                        Sorting.switchUsePlan.All
                        Sorting.eventGrouping.NoGrouping
                        (UseParallel.create true)
                        (SortingEval.SorterCoverage.fromSwitchEventRecords true)
                    |> Result.ExtractOrThrow


        let getSorterLength (sc:SortingEval.sorterCoverage) =
            subSorterSet.sorters.[sc.sorterId].switchCount
            |> SwitchCount.value

        let report (sc:SortingEval.sorterCoverage) =
           sprintf "%d\t%s" (getSorterLength sc) 
                            (SortingEval.SorterPerf.report sc.perf)


        let winners =
            ssR |> List.filter(fun sc -> SortingEval.SorterPerf.isSucessful sc.perf)
        
        winners |> List.iter(fun sc -> Debug.WriteLine (report sc))

        Assert.IsTrue(subSorters.Length > 0)