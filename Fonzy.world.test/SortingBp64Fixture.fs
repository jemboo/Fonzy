namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SortingEval

[<TestClass>]
type SortingBp64Fixture () =

    [<TestMethod>]
    member this.sorterGrouping() =
        let refSorter = TestData.SorterParts.mediocreRandomSorters
                        |> List.head

        let resGroupBySwitch = 
            SortingBp64.sorterMakeSwitchUses
                refSorter
                TestData.SorterActionRecords.bP64SetsRolloutOfAll
                Sorting.switchUsePlan.All

        let resNoGrouping = 
            SortingBp64.sorterWithNoSAG 
                refSorter
                TestData.SorterActionRecords.bP64SetsRolloutOfAll
                Sorting.switchUsePlan.All

        let switchUsesGrouping = 
                resGroupBySwitch
                    |> SortingEval.SwitchEventRecords.getSwitchUses

        let switchUsesNoGrouping = 
                resNoGrouping
                    |> SortingEval.SwitchEventRecords.getSwitchUses

        let usedSwitchCountGrouping = switchUsesGrouping 
                                      |> SwitchUses.usedSwitchCount

        let usedSwitchCountNoGrouping = switchUsesNoGrouping 
                                        |> SwitchUses.usedSwitchCount

        Assert.AreEqual(usedSwitchCountGrouping, usedSwitchCountNoGrouping)


    //[<TestMethod>]
    //member this.getHistogramOfSortedSortables() =
    //    let refSorter = TestData.SorterParts.goodRefSorter

    //    let switchEventRecordsNoSAG = 
    //        SortingBp64.sorterWithNoSAG 
    //            refSorter 
    //            TestData.SorterActionRecords.bP64SetsRolloutOfAll
    //            Sorting.switchUsePlan.All
   
    //    let sortedSortablesNoSAG = 
    //            switchEventRecordsNoSAG
    //                |> SortingEval.SwitchEventRecords.getHistogramOfSortedSortables
    //                |> Array.toList

    //    Assert.AreEqual(sortedSortablesNoSAG.Length, (Degree.value refSorter.degree))


    //    let switchEventRecordsMakeSwitchUses = 
    //        SortingBp64.sorterMakeSwitchUses 
    //            refSorter 
    //            TestData.SorterActionRecords.bP64SetsRolloutOfAll
    //            Sorting.switchUsePlan.All
   
    //    let sortedSortablesMakeSwitchUses = 
    //            switchEventRecordsMakeSwitchUses
    //                |> SortingEval.SwitchEventRecords.getHistogramOfSortedSortables
    //                |> Array.toList

    //    Assert.AreEqual(sortedSortablesMakeSwitchUses.Length, (Degree.value refSorter.degree))



    //[<TestMethod>]
    //member this.evalSorter() =
    //    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    //    let sorter16 = RefSorter.goodRefSorterForDegree degree 
    //                    |> Result.ExtractOrThrow
    //    let sst = sortableSetType.AllForDegree
    //                    (sortableSetRep.Bp64 sorter16.degree)
    //    let srtblSt = SortableSetMaker.makeNoRepo sst
    //                  |> Result.ExtractOrThrow

    //    let switchEventRecords = 
    //                    SortingBp64.evalSorter 
    //                        sorter16 
    //                        ssBp64.sortables
    //                        Sorting.switchUsePlan.All
    //                        Sorting.eventGrouping.BySwitch

    //    let usedSwitchCount = switchEventRecords 
    //                          |> SwitchEventRecords.getUsedSwitchCount
    //                          |> Result.ExtractOrThrow

    //    Assert.IsTrue((SwitchCount.value usedSwitchCount) > 0)


    [<TestMethod>]
    member this.Hist() =
        let testCase = seq { TestData.SorterParts.randomIntBits }
                       |> BitsP64.fromBitSet
                       |> Seq.head
        let goodSorter = TestData.SorterParts.goodRefSorter
        let hist = SortingBp64.History.sortTHist goodSorter testCase
        Assert.AreEqual(hist.Length, 1 + SwitchCount.value goodSorter.switchCount)
        let result = hist.Item (hist.Length - 1)
        Assert.IsTrue(result |> BitsP64.isSorted)


    [<TestMethod>]
    member this.SorterSet_eval() =
        let sorterSet = TestData.SorterSet.mediocreSorterSet
        let sst = sortableSetType.AllForDegree
                        (sortableSetRep.Binary sorterSet.degree)
        let srtblSt = SortableSetMaker.makeNoRepo sst
                      |> Result.ExtractOrThrow

        let ssRBp = SortingOps.SorterSet.eval2
                        sorterSet 
                        srtblSt 
                        Sorting.switchUsePlan.All
                        Sorting.eventGrouping.BySwitch
                        (UseParallel.create true)
                        (SortingEval.SorterCoverage.fromSwitchEventRecords true)
                        |> Result.ExtractOrThrow

        Assert.AreEqual(SorterCount.value sorterSet.sorterCount, ssRBp.Length)

        
    [<TestMethod>]
    member this.SorterSet_evalCompBp() =
        let sorterSet = TestData.SorterSet.mediocreSorterSet

        let sstBp = sortableSetType.AllForDegree
                        (sortableSetRep.Bp64 sorterSet.degree)
        let srtblStBp = SortableSetMaker.makeNoRepo sstBp
                        |> Result.ExtractOrThrow

        let ssRBp = SortingOps.SorterSet.eval2
                        sorterSet 
                        srtblStBp 
                        Sorting.switchUsePlan.All
                        Sorting.eventGrouping.BySwitch
                        (UseParallel.create true)
                        (SortingEval.SorterCoverage.fromSwitchEventRecords true)
                        |> Result.ExtractOrThrow
                        |> List.map(fun sc -> sc.perf)

        let sstInt = sortableSetType.AllForDegree
                        (sortableSetRep.Integer sorterSet.degree)
        let srtblStInt = SortableSetMaker.makeNoRepo sstInt
                        |> Result.ExtractOrThrow

        let ssR = SortingOps.SorterSet.eval2
                        sorterSet 
                        srtblStInt 
                        Sorting.switchUsePlan.All
                        Sorting.eventGrouping.BySwitch
                        (UseParallel.create true)
                        (SortingEval.SorterCoverage.fromSwitchEventRecords true)
                        |> Result.ExtractOrThrow
                        |> List.map(fun sc -> sc.perf)

        Assert.AreEqual(ssR, ssRBp)

