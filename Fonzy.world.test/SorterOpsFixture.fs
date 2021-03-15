namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SorterOpsFixture () =

    //[<TestMethod>]
    //member this.SAGbySwitch() =
    //    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    //    let sorter16 = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
    //    let sortableSet = SortableSetRollout.allBinary degree |> Result.ExtractOrThrow    

    //    let resR = SortingOps.EvalSorterOnSortableSetSAGbySwitch 
    //                    sorter16 sortableSet SortingEval.SwitchUsePlan.All
    //    //let wR = snd resR    
    //    //let histo = wR |> SortableSetRollout.toSortableIntArrays
    //    //               |> Seq.countBy id
    //    //               |> Seq.toArray
    //    //Assert.IsTrue(histo.Length > 1)
    //    Assert.IsTrue(true)

    [<TestMethod>]
    member this.evalAndGetSwitchUses() =
        let refSorter = TestData.SorterParts.goodRefSorter
        let sortableSet = TestData.SortableSet.ssAllIntBits 

        let resGroupBySwitch = 
            SortingOps.evalGroupBySwitch
                TestData.SorterParts.goodRefSorter 
                TestData.SorterActionRecords.rolloutOfAllBinary
                SortingEval.SwitchUsePlan.All

        let resNoGrouping = 
            SortingOps.evalNoGrouping 
                TestData.SorterParts.goodRefSorter 
                TestData.SorterActionRecords.rolloutOfAllBinary
                SortingEval.SwitchUsePlan.All
        
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
                SortingEval.SwitchUsePlan.All

        let resNoGrouping = 
            SortingOps.evalNoGrouping 
                TestData.SorterParts.goodRefSorter 
                TestData.SorterActionRecords.rolloutOfAllBinary
                SortingEval.SwitchUsePlan.All
    
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
    member this.evalRecordCorrectSorter() =
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let sorter16 = RefSorter.goodRefSorterForDegree degree |> Result.ExtractOrThrow
        let sortableSetEx = SortableSet.Generated (SortableSetGenerated.allIntBits degree)
                                |> SortableSet.getSortableSetExplicit
                                |> Result.ExtractOrThrow 

        let ssR = SortingOps.evalSorter 
                            sorter16 sortableSetEx SortingEval.SwitchUsePlan.All
                            SortingEval.SwitchEventGrouping.BySwitch
        let switchCount =
            match ssR with
            | SortingEval.SwitchEventRecords.GroupbySwitch s -> s.switchUses.switchCount
            | _ -> failwith "yoe"
        Assert.IsTrue((SwitchCount.value switchCount) > 0)