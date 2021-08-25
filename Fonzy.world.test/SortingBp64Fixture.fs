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


    [<TestMethod>]
    member this.getHistogramOfSortedSortables() =
        let refSorter = TestData.SorterParts.goodRefSorter

        let switchEventRecordsNoSAG = 
            SortingBp64.sorterWithNoSAG 
                refSorter 
                TestData.SorterActionRecords.bP64SetsRolloutOfAll
                Sorting.switchUsePlan.All
   
        let sortedSortablesNoSAG = 
                switchEventRecordsNoSAG
                    |> SortingEval.SwitchEventRecords.getHistogramOfSortedSortables
                    |> Array.toList

        Assert.AreEqual(sortedSortablesNoSAG.Length, (Degree.value refSorter.degree))


        let switchEventRecordsMakeSwitchUses = 
            SortingBp64.sorterMakeSwitchUses 
                refSorter 
                TestData.SorterActionRecords.bP64SetsRolloutOfAll
                Sorting.switchUsePlan.All
   
        let sortedSortablesMakeSwitchUses = 
                switchEventRecordsMakeSwitchUses
                    |> SortingEval.SwitchEventRecords.getHistogramOfSortedSortables
                    |> Array.toList

        Assert.AreEqual(sortedSortablesMakeSwitchUses.Length, (Degree.value refSorter.degree))



    [<TestMethod>]
    member this.evalSorter() =
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let sorter16 = RefSorter.goodRefSorterForDegree degree 
                        |> Result.ExtractOrThrow
        let ssBp64 = SortableSetBp64.allBp64 degree

        let switchEventRecords = 
                        SortingBp64.evalSorter 
                            sorter16 
                            ssBp64
                            Sorting.switchUsePlan.All
                            Sorting.eventGrouping.BySwitch

        let usedSwitchCount = switchEventRecords 
                              |> SwitchEventRecords.getUsedSwitchCount
                              |> Result.ExtractOrThrow

        Assert.IsTrue((SwitchCount.value usedSwitchCount) > 0)


    [<TestMethod>]
    member this.Hist() =
        let testCase = seq { TestData.SorterParts.randomIntBits }
                       |> BitsP64.fromIntBits
                       |> Seq.head
        let goodSorter = TestData.SorterParts.goodRefSorter
        let hist = SortingBp64.History.sortTHist goodSorter testCase
        Assert.AreEqual(hist.Length, 1 + SwitchCount.value goodSorter.switchCount)
        let result = hist.Item (hist.Length - 1)
        Assert.IsTrue(result |> BitsP64.isSorted)


    [<TestMethod>]
    member this.SorterSet_eval() =
        let sorterSet = TestData.SorterSet.mediocreSorterSet
        let sortableSetBps = sortableSetSpec.Generated 
                                 (SortableSetGen.allBp64 sorterSet.degree)
                                 |> SortableSetSpec.getSortableSet
                                 |> Result.ExtractOrThrow

        let ssRBp = SortingOps.SorterSet.eval
                        sorterSet 
                        sortableSetBps 
                        Sorting.switchUsePlan.All
                        Sorting.eventGrouping.BySwitch
                        (UseParallel.create true)
                        (SortingEval.SorterCoverage.fromSwitchEventRecords true)
                        |> Result.ExtractOrThrow

        Assert.AreEqual(SorterCount.value sorterSet.sorterCount, ssRBp.Length)

        
    [<TestMethod>]
    member this.SorterSet_evalCompBp() =
        let sorterSet = TestData.SorterSet.mediocreSorterSet
        let sortableSetBps = sortableSetSpec.Generated 
                                 (SortableSetGen.allBp64 sorterSet.degree)
                                 |> SortableSetSpec.getSortableSet
                                 |> Result.ExtractOrThrow

        let ssRBp = SortingOps.SorterSet.eval
                        sorterSet 
                        sortableSetBps 
                        Sorting.switchUsePlan.All
                        Sorting.eventGrouping.BySwitch
                        (UseParallel.create true)
                        (SortingEval.SorterCoverage.fromSwitchEventRecords true)
                        |> Result.ExtractOrThrow


        let sortableSetBinary = sortableSetSpec.Generated 
                                    (SortableSetGen.allIntBits sorterSet.degree)
                                 |> SortableSetSpec.getSortableSet
                                 |> Result.ExtractOrThrow 

        let ssR = SortingOps.SorterSet.eval
                        sorterSet 
                        sortableSetBinary 
                        Sorting.switchUsePlan.All
                        Sorting.eventGrouping.BySwitch
                        (UseParallel.create true)
                        (SortingEval.SorterCoverage.fromSwitchEventRecords true)
                        |> Result.ExtractOrThrow

        Assert.AreEqual(ssR, ssRBp)

