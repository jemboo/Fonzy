namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SorterOpsFixture () =

    [<TestMethod>]
    member this.evalAndGetSwitchUses() =
        let refSorter = TestData.SorterParts.goodRefSorter
        let sortableSet = TestData.SortableSet.ssAllIntBits 

        let resGroupBySwitch = 
            SortingOps.evalGroupBySwitch
                TestData.SorterParts.goodRefSorter 
                TestData.SorterActionRecords.rolloutOfAllBinary
                Sorting.SwitchUsePlan.All

        let resNoGrouping = 
            SortingOps.evalNoGrouping 
                TestData.SorterParts.goodRefSorter 
                TestData.SorterActionRecords.rolloutOfAllBinary
                Sorting.SwitchUsePlan.All
        
        let switchUsesGrouping = 
                resGroupBySwitch
                    |> SortingEval.SwitchEventRecords.getSwitchUses
                    |> Result.ExtractOrThrow

        let switchUsesNoGrouping = 
                resNoGrouping
                    |> SortingEval.SwitchEventRecords.getSwitchUses
                    |> Result.ExtractOrThrow

        let usedSwitchCount = refSorter  
                                |> SwitchUses.getUsedSwitches switchUsesGrouping
                                |> Result.ExtractOrThrow

        Assert.AreEqual(switchUsesGrouping, switchUsesNoGrouping)
        Assert.AreEqual(usedSwitchCount.Length, (SwitchCount.value refSorter.switchCount))


    [<TestMethod>]
    member this.evalAndGetSortableUses() =
        let refSorter = TestData.SorterParts.goodRefSorter
        let sortableSet = TestData.SortableSet.ssAllIntBits 

        let resGroupBySortable = 
            SortingOps.evalGroupBySortable
                TestData.SorterParts.goodRefSorter 
                TestData.SorterActionRecords.rolloutOfAllBinary
                Sorting.SwitchUsePlan.All

        let resNoGrouping = 
            SortingOps.evalNoGrouping 
                TestData.SorterParts.goodRefSorter 
                TestData.SorterActionRecords.rolloutOfAllBinary
                Sorting.SwitchUsePlan.All
    
        let sortedSortablesGrouping = 
            resGroupBySortable
                |> SortingEval.SwitchEventRecords.getHistogramOfSortedSortables
                |> Result.ExtractOrThrow

        let sortedSortablesNoGrouping = 
                resNoGrouping
                    |> SortingEval.SwitchEventRecords.getHistogramOfSortedSortables
                    |> Result.ExtractOrThrow

        Assert.AreEqual(sortedSortablesGrouping, sortedSortablesNoGrouping)
        Assert.AreEqual(sortedSortablesNoGrouping.Length, (Degree.value refSorter.degree) + 1)


    [<TestMethod>]
    member this.evalSorter() =
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let sorter16 = RefSorter.goodRefSorterForDegree degree 
                        |> Result.ExtractOrThrow
        let sortableSetEx = SortableSet.Generated (SortableSetGenerated.allIntBits degree)
                                |> SortableSet.getSortableSetExplicit
                                |> Result.ExtractOrThrow 

        let ssR = SortingOps.evalSorter 
                        sorter16 sortableSetEx Sorting.SwitchUsePlan.All
                        Sorting.EventGrouping.BySwitch
        let switchCount =
            match ssR with
            | SortingEval.SwitchEventRecords.BySwitch s -> s.switchUses.switchCount
            | _ -> failwith "yoe"
        Assert.IsTrue((SwitchCount.value switchCount) > 0)



    [<TestMethod>]
    member this.HistAndHist2() =
        let testCase = TestData.SorterParts.randomSortableIntArray
        let goodSorter = TestData.SorterParts.goodRefSorter

        let hist = SortingOps.History.sortTHist goodSorter testCase
        Assert.IsTrue(hist.Length > 1)
        let result = hist.Item (hist.Length - 1)
        Assert.IsTrue(result |> SortableIntArray.isSorted)

        let hist2 = SortingOps.History.sortTHist goodSorter testCase
        Assert.IsTrue(hist2.Length > 1)
        let result2 = hist2.Item (hist2.Length - 1)
        Assert.IsTrue(result2 |> SortableIntArray.isSorted)

        Assert.AreEqual(result, result2)
