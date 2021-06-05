namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SortingEval

[<TestClass>]
type SortingOpsFixture () =

    [<TestMethod>]
    member this.evalAndGetSwitchUses() =
        let refSorter = TestData.SorterParts.goodRefSorter
        let sortableSet = TestData.SortableSet.ssBinary 

        let resGroupBySwitch = 
            SortingInts.sorterMakeSwitchUses
                TestData.SorterParts.goodRefSorter 
                TestData.SorterActionRecords.intSetsRolloutAllBinary
                Sorting.SwitchUsePlan.All

        let resNoGrouping = 
            SortingInts.sorterWithNoSAG 
                TestData.SorterParts.goodRefSorter 
                TestData.SorterActionRecords.intSetsRolloutAllBinary
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
    member this.getHistogramOfSortedSortables() =
        let refSorter = TestData.SorterParts.goodRefSorter

        let switchEventRecords = 
            SortingInts.sorterWithNoSAG 
                refSorter 
                TestData.SorterActionRecords.intSetsRolloutAllBinary
                Sorting.SwitchUsePlan.All
   

        let sortedSortablesNoGrouping = 
                switchEventRecords
                    |> SortingEval.SwitchEventRecords.getHistogramOfSortedSortables
                    |> Result.ExtractOrThrow
                    |> Array.toList

        Assert.AreEqual(sortedSortablesNoGrouping.Length, (Degree.value refSorter.degree) + 1)


    [<TestMethod>]
    member this.evalSorter() =
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let sorter16 = RefSorter.goodRefSorterForDegree degree 
                        |> Result.ExtractOrThrow
        let sortableSetBinary = SortableSetBinary.allIntBits degree

        let ssR = SortingInts.evalSorter 
                        sorter16 
                        sortableSetBinary
                        Sorting.SwitchUsePlan.All
                        Sorting.EventGrouping.BySwitch
        let switchCount =
            match ssR with
            | SortingEval.SwitchEventRecords.BySwitch s -> s.switchUses.switchCount
            | _ -> failwith "yoe"
        Assert.IsTrue((SwitchCount.value switchCount) > 0)


    [<TestMethod>]
    member this.Hist() =
        let testCase = TestData.SorterParts.randomIntBits
        let goodSorter = TestData.SorterParts.goodRefSorter
        let hist = SortingInts.History.sortTHist goodSorter testCase
        Assert.IsTrue(hist.Length > 1)
        let result = hist.Item (hist.Length - 1)
        Assert.IsTrue(result |> IntBits.isSorted)


    [<TestMethod>]
    member this.Hist2() =
        let testCase = TestData.SorterParts.randomBitsP64
        let goodSorter = TestData.SorterParts.goodRefSorter
        let hist = SortingBp64.History.sortTHist goodSorter testCase
        Assert.IsTrue(hist.Length > 1)
        let result = hist.Item (hist.Length - 1)
        Assert.IsTrue(result |> BitsP64.isSorted)



    [<TestMethod>]
    member this.evalSorter2() =
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let sorter16 = RefSorter.goodRefSorterForDegree degree 
                        |> Result.ExtractOrThrow

        let sortableSetEx = SortableSetBp64.allIntBits degree

        let ssR = SortingBp64.evalSorter 
                        sorter16 sortableSetEx Sorting.SwitchUsePlan.All
                        Sorting.EventGrouping.BySwitch
        let switchCount =
            match ssR with
            | SortingEval.SwitchEventRecords.BySwitch s -> 
                                 SwitchUses.usedSwitchCount s.switchUses
                                 |> Ok
            | _ -> failwith "yoe"
            |> Result.ExtractOrThrow

        Assert.IsTrue((SwitchCount.value switchCount) > 59)



    [<TestMethod>]
    member this.SorterSet_eval2() =

        let sorterSet = TestData.SorterSet.mediocreSorterSet

        let sortableSetEx = SortableSetBp64.allIntBits sorterSet.degree

        //let ssR = SortingBp64.SorterSet.eval
        //                sorterSet 
        //                sortableSetEx 
        //                Sorting.SwitchUsePlan.All
        //                Sorting.EventGrouping.BySwitch
        //                (UseParallel.create true)
        //                SortingEval.SortingRecords.getSorterCoverage
        //            |> Result.ExtractOrThrow
        Assert.IsTrue(true)



    [<TestMethod>]
    member this.SorterSet_eval() =
        let sorterSet = TestData.SorterSet.mediocreSorterSet
        let sortableSetBinary = SortableSetSpec.Generated 
                                 (SortableSetGenerated.allIntBits sorterSet.degree)
                                 |> SortableSetSpec.getSortableSetExplicit
                                 |> Result.ExtractOrThrow 
        let ssR = SortingOps.SorterSet.eval
                        sorterSet 
                        sortableSetBinary 
                        Sorting.SwitchUsePlan.All
                        Sorting.EventGrouping.BySwitch
                        (UseParallel.create true)
                        SortingEval.SortingRecords.getSorterCoverage
        Assert.IsTrue(true)

        
        
    [<TestMethod>]
    member this.getSorterEff_Parallel_NoGrouping() =
        let seed = 1234
        let rngGen = (RngGen.createLcg seed)
        let iRando = Rando.fromRngGen rngGen
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let mediocreSorterSetId = SorterSetId.fromGuid (Guid.NewGuid())
        let altEvenSorterSetId = SorterSetId.fromGuid (Guid.NewGuid())

        let sorterLength = degree |> SwitchOrStageCount.toMediocreRandomPerfLength 
                                                    SwitchOrStage.Stage
        let stageCount = degree |> StageCount.degreeTo999StageCount
        let sorterCount = SorterCount.fromInt 500

        let maxConjugatePairs = 100
        let altEvenSorters = List.init maxConjugatePairs (fun stageCt -> 
             SorterGen.makeAltEvenOdd degree (StageCount.fromInt (stageCt + 5)))
                             |> Result.sequence
                             |> Result.ExtractOrThrow

        let altEvenSorterSet = 
                    SorterSet.fromSorters 
                            altEvenSorterSetId
                            degree 
                            altEvenSorters

        let makeCoConjSorter (perms:Permutation list) = 
            result {
                let! stp = perms |> TwoCycleGen.makeCoConjugateEvenOdd
                let atp = stp |> Seq.toArray
                return SorterGen.fromTwoCycleArray atp
            }
            
        let makeRandomSorter() = 
            let sorterGen = SorterGen.RandStages (stageCount, degree)
            SorterGen.createRandom sorterGen

        let mediocreRandomSorters = 
            List.init (SorterCount.value sorterCount)
                      (fun _ -> makeRandomSorter())

        let sortableSetEx = SortableSetSpec.Generated 
                                (SortableSetGenerated.allIntBits degree)
                                |> SortableSetSpec.getSortableSetExplicit
                                |> Result.ExtractOrThrow

        let perfBins = SortingOps.SorterSet.getSorterCoverageBins
                          altEvenSorterSet
                          sortableSetEx
                          Sorting.SwitchUsePlan.All
                          (UseParallel.create true)
                          
        let yab  = perfBins |> Result.ExtractOrThrow
        let ct = yab |> Array.sumBy(snd)
        Assert.IsTrue(ct > 0)
