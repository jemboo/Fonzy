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
                Sorting.switchUsePlan.All

        let resNoGrouping = 
            SortingInts.sorterWithNoSAG 
                TestData.SorterParts.goodRefSorter 
                TestData.SorterActionRecords.intSetsRolloutOfAll
                Sorting.switchUsePlan.All
        
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
                Sorting.switchUsePlan.All
   

        //let sortedSortablesNoSAG = 
        //        switchEventRecordsNoSAG
        //            |> SortingEval.SwitchEventRecords.    //.getHistogramOfSortedSortables
        //            |> Array.toList

        //Assert.AreEqual(sortedSortablesNoSAG.Length, (Degree.value refSorter.degree) + 1)

        //let switchEventRecordsMakeSwitchUses = 
        //    SortingInts.sorterMakeSwitchUses 
        //        refSorter 
        //        TestData.SorterActionRecords.intSetsRolloutOfAll
        //        Sorting.switchUsePlan.All
   

        //let sortedSortablesMakeSwitchUses = 
        //        switchEventRecordsMakeSwitchUses
        //            |> SortingEval.SwitchEventRecords.getHistogramOfSortedSortables
        //            |> Array.toList

        //Assert.AreEqual(sortedSortablesMakeSwitchUses.Length, (Degree.value refSorter.degree) + 1)
        Assert.IsTrue(true)


    //[<TestMethod>]
    //member this.evalSorter() =
    //    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    //    let sorter16 = RefSorter.goodRefSorterForDegree degree 
    //                    |> Result.ExtractOrThrow
    //    let sortableSetBinary = SortableSetBinary.allIntBits degree
        
    //    let switchEventRecords = 
    //                SortingInts.evalSorterOnBinary 
    //                    sorter16 
    //                    sortableSetBinary.sortables
    //                    Sorting.switchUsePlan.All
    //                    Sorting.eventGrouping.BySwitch

    //    let usedSwitchCount = switchEventRecords 
    //                          |> SwitchEventRecords.getUsedSwitchCount
    //                          |> Result.ExtractOrThrow

    //    Assert.IsTrue((SwitchCount.value usedSwitchCount) > 0)


    [<TestMethod>]
    member this.Hist() =
        let testCase = TestData.SorterParts.randomIntBits |> BitSet.toIntSet
        let goodSorter = TestData.SorterParts.goodRefSorter
        let hist = SortingInts.History.sortTHist goodSorter testCase
        Assert.AreEqual(hist.Length, 1 + SwitchCount.value goodSorter.switchCount)
        let result = hist.Item (hist.Length - 1)
        Assert.IsTrue(result |> IntSet.isSorted)



    [<TestMethod>]
    member this.SorterSet_eval() =
        let sorterSet = TestData.SorterSet.mediocreSorterSet
        let srtblStType = sortableSetType.AllForDegree 
                                   (sortableSetRep.Bp64 sorterSet.degree)
        let srtableSet = SortableSetMaker.makeNoRepo srtblStType
                         |> Result.ExtractOrThrow

        let ssR = SortingOps.SorterSet.eval
                        sorterSet 
                        srtableSet 
                        Sorting.switchUsePlan.All
                        Sorting.eventGrouping.BySwitch
                        (UseParallel.create true)
                        (SortingEval.SorterCoverage.fromSwitchEventRecords true)
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

        let makeCoConjSorter (perms:permutation list) = 
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

        let sstInt = sortableSetType.AllForDegree
                        (sortableSetRep.Integer degree)
        let srtblStInt = SortableSetMaker.makeNoRepo sstInt
                         |> Result.ExtractOrThrow

        let sorterCovs = SortingOps.SorterSet.getSorterCoverages
                              altEvenSorterSet
                              srtblStInt
                              Sorting.switchUsePlan.All
                              true
                              (UseParallel.create true)
                         |> Result.ExtractOrThrow


        let perfBins = sorterCovs 
                        |> SortingEval.SorterPerfBin.fromSorterCoverages
                          
        let ct = perfBins |> Array.sumBy(fun spb -> (SorterCount.value spb.sorterCount))
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

        let sSet = SorterSet.fromSorters 
                            sorterSetId
                            degree 
                            sorterArray

        //let sortableSetEx = sortableSetSpec.Generated 
        //                        (SortableSetGen.allIntBits degree)
        //                        |> SortableSetSpec.getSortableSet
        //                        |> Result.ExtractOrThrow 

        //let sorterCovs = SortingOps.SorterSet.getSorterCoverages
        //                    sSet
        //                    sortableSetEx
        //                    Sorting.switchUsePlan.All
        //                    true
        //                    (UseParallel.create true)
        //                |> Result.ExtractOrThrow

        //let perfBins = sorterCovs 
        //                |> SortingEval.SorterPerfBin.fromSorterCoverages

        //let pbr  = perfBins |> Result.ExtractOrThrow
        //let rep = (pbr |> SorterPerf.binReport)
        //let tot = pbr |> Array.sumBy(snd)
        //Console.WriteLine (sprintf "tot:%d" tot)
        //Console.WriteLine ""
        //Console.WriteLine rep

        Assert.IsTrue(true)
