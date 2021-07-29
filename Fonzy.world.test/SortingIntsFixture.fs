namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SortingEval

[<TestClass>]
type SortingIntsFixture () =

    [<TestMethod>]
    member this.sorterGrouping() =
        let refSorter = TestData.SorterParts.goodRefSorter

        let resGroupBySwitch = 
            SortingInts.sorterMakeSwitchUses
                TestData.SorterParts.goodRefSorter 
                TestData.SorterActionRecords.intSetsRolloutOfAll
                Sorting.SwitchUsePlan.All

        let resNoGrouping = 
            SortingInts.sorterWithNoSAG 
                TestData.SorterParts.goodRefSorter 
                TestData.SorterActionRecords.intSetsRolloutOfAll
                Sorting.SwitchUsePlan.All
        
        let switchUsesGrouping = 
                resGroupBySwitch
                    |> SortingEval.SwitchEventRecords.getSwitchUses

        let switchUsesNoGrouping = 
                resNoGrouping
                    |> SortingEval.SwitchEventRecords.getSwitchUses

        let usedSwitchCount = refSorter  
                                |> SwitchUses.getUsedSwitches switchUsesGrouping

        Assert.AreEqual(switchUsesGrouping, switchUsesNoGrouping)
        Assert.AreEqual(usedSwitchCount.Length, (SwitchCount.value refSorter.switchCount))


    [<TestMethod>]
    member this.getHistogramOfSortedSortables() =
        let refSorter = TestData.SorterParts.goodRefSorter

        let switchEventRecordsNoSAG = 
            SortingInts.sorterWithNoSAG 
                refSorter 
                TestData.SorterActionRecords.intSetsRolloutOfAll
                Sorting.SwitchUsePlan.All
   

        let sortedSortablesNoSAG = 
                switchEventRecordsNoSAG
                    |> SortingEval.SwitchEventRecords.getHistogramOfSortedSortables
                    |> Array.toList

        Assert.AreEqual(sortedSortablesNoSAG.Length, (Degree.value refSorter.degree) + 1)

        let switchEventRecordsMakeSwitchUses = 
            SortingInts.sorterMakeSwitchUses 
                refSorter 
                TestData.SorterActionRecords.intSetsRolloutOfAll
                Sorting.SwitchUsePlan.All
   

        let sortedSortablesMakeSwitchUses = 
                switchEventRecordsMakeSwitchUses
                    |> SortingEval.SwitchEventRecords.getHistogramOfSortedSortables
                    |> Array.toList

        Assert.AreEqual(sortedSortablesMakeSwitchUses.Length, (Degree.value refSorter.degree) + 1)



    [<TestMethod>]
    member this.evalSorter() =
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let sorter16 = RefSorter.goodRefSorterForDegree degree 
                        |> Result.ExtractOrThrow
        let sortableSetBinary = SortableSetBinary.allIntBits degree
        
        let switchEventRecords = 
                    SortingInts.evalSorterOnBinary 
                        sorter16 
                        sortableSetBinary
                        Sorting.SwitchUsePlan.All
                        Sorting.EventGrouping.BySwitch

        let usedSwitchCount = switchEventRecords 
                              |> SwitchEventRecords.getUsedSwitchCount
                              |> Result.ExtractOrThrow

        Assert.IsTrue((SwitchCount.value usedSwitchCount) > 0)


    [<TestMethod>]
    member this.Hist() =
        let testCase = TestData.SorterParts.randomIntBits
        let goodSorter = TestData.SorterParts.goodRefSorter
        let hist = SortingInts.History.sortTHist goodSorter testCase
        Assert.AreEqual(hist.Length, 1 + SwitchCount.value goodSorter.switchCount)
        let result = hist.Item (hist.Length - 1)
        Assert.IsTrue(result |> IntBits.isSorted)



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
                        (SortingEval.SortingRecords.getSorterCoverage true)
                        |> Result.ExtractOrThrow

        Assert.AreEqual(SorterCount.value sorterSet.sorterCount, ssR.Length)

        


    //// Scraps
        
    [<TestMethod>]
    member this.getSorterEff_Parallel_NoGrouping() =
        let seed = 1234 |> RandomSeed.fromInt
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
            let sorterGen = sorterRndGen.RandStages ([], stageCount, degree)
            SorterRndGen.createRandom sorterGen

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
                          true
                          (UseParallel.create true)
                          
        let yab  = perfBins |> Result.ExtractOrThrow
        let ct = yab |> Array.sumBy(fun spb -> (SorterCount.value spb.sorterCount))
        Assert.IsTrue(ct > 0)



    [<TestMethod>]
    member this.makeTreeSorter() =
        let seed = 12345 |> RandomSeed.fromInt
        let iRando = Rando.fromRngGen (RngGen.createLcg seed)
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let sorterSetId = SorterSetId.fromGuid (Guid.NewGuid())
               
        let sorterLength = SwitchOrStageCount.makeStageCount 400
        let stageCount = sorterLength |> SwitchOrStageCount.getStageCount
                                      |> Result.ExtractOrThrow
   
        let sorterCount = SorterCount.fromInt 10

        let makeSorter() = 
            let perms2 = List.init 
                            (StageCount.value stageCount)
                            (fun _ -> sTree.makePerm 0.29 iRando 4)
                            |> Result.sequence |> Result.ExtractOrThrow
                            |> List.toArray
            SorterRndGen.fromTwoCyclePerms Seq.empty perms2

        let sorterArray = Array.init 
                               (SorterCount.value sorterCount)
                               (fun _ -> makeSorter ())  

        let sorterSet = 
                    SorterSet.fromSorters 
                            sorterSetId
                            degree 
                            sorterArray

        let sortableSetEx = SortableSetSpec.Generated 
                                (SortableSetGenerated.allIntBits degree)
                                |> SortableSetSpec.getSortableSetExplicit
                                |> Result.ExtractOrThrow 

        let perfBins = SortingOps.SorterSet.getSorterCoverageBins
                            sorterSet
                            sortableSetEx
                            Sorting.SwitchUsePlan.All
                            true
                            (UseParallel.create true)

        //let pbr  = perfBins |> Result.ExtractOrThrow
        //let rep = (pbr |> SorterPerf.binReport)
        //let tot = pbr |> Array.sumBy(snd)
        //Console.WriteLine (sprintf "tot:%d" tot)
        //Console.WriteLine ""
        //Console.WriteLine rep

        Assert.IsTrue(true)
