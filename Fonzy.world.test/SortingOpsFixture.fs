namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SortingEval

[<TestClass>]
type SortingOpsFixture () =

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
                |> Array.toList

        let sortedSortablesNoGrouping = 
                resNoGrouping
                    |> SortingEval.SwitchEventRecords.getHistogramOfSortedSortables
                    |> Result.ExtractOrThrow
                    |> Array.toList

        Assert.AreEqual(sortedSortablesGrouping, sortedSortablesNoGrouping)
        Assert.AreEqual(sortedSortablesNoGrouping.Length, (Degree.value refSorter.degree) + 1)


    [<TestMethod>]
    member this.evalSorter() =
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let sorter16 = RefSorter.goodRefSorterForDegree degree 
                        |> Result.ExtractOrThrow
        let sortableSetEx = SortableSet.Generated 
                                (SortableSetGenerated.allIntBits degree)
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
    member this.Hist() =
        let testCase = TestData.SorterParts.randomIntBits
        let goodSorter = TestData.SorterParts.goodRefSorter
        let hist = SortingOps.History.sortTHist goodSorter testCase
        Assert.IsTrue(hist.Length > 1)
        let result = hist.Item (hist.Length - 1)
        Assert.IsTrue(result |> IntBits.isSorted)


    [<TestMethod>]
    member this.Hist2() =
        let testCase = TestData.SorterParts.randomBitsP32
        let goodSorter = TestData.SorterParts.goodRefSorter
        let hist = SortingOps2.History.sortTHist2 goodSorter testCase
        Assert.IsTrue(hist.Length > 1)
        let result = hist.Item (hist.Length - 1)
        Assert.IsTrue(result |> bitsP32.isSorted)


    [<TestMethod>]
    member this.SorterSet_eval() =
        let sorterSet = TestData.SorterSet.mediocreSorterSet
        let sortableSetEx = SortableSet.Generated 
                                (SortableSetGenerated.allIntBits sorterSet.degree)
                                |> SortableSet.getSortableSetExplicit
                                |> Result.ExtractOrThrow 
        let ssR = SortingOps.SorterSet.eval
                        sorterSet 
                        sortableSetEx 
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

        let sortableSetEx = SortableSet.Generated 
                                (SortableSetGenerated.allIntBits degree)
                                |> SortableSet.getSortableSetExplicit
                                |> Result.ExtractOrThrow

        let perfBins = SortingOps.SorterSet.getSorterPerfBins
                          altEvenSorterSet
                          sortableSetEx
                          Sorting.SwitchUsePlan.All
                          (UseParallel.create true)
                          
        let yab  = perfBins |> Result.ExtractOrThrow
        let ct = yab |> Array.sumBy(snd)
        Assert.IsTrue(ct > 0)



    [<TestMethod>]
    member this.makeCoConjSorter() =
        let seed = 72345
        let iRando = Rando.fromRngGen (RngGen.createLcg seed)
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let coConjSorterSetId = SorterSetId.fromGuid (Guid.NewGuid())

        //let sorterLength = degree |> SwitchOrStageCount.toMediocreRandomPerfLength 
        //                                            SwitchOrStage.Stage
        let sorterLength = SwitchOrStageCount.makeStageCount 155

        let stageCount = sorterLength |> SwitchOrStageCount.getStageCount
                                      |> Result.ExtractOrThrow

        let sorterCount = SorterCount.fromInt 10

        let makeCoConjSorter() = 
            //let perms = //seq {Permutation.identity degree; 
            //            seq {
            //                 yield! (Permutation.createRandoms degree iRando)}
            //            |> Seq.take( (StageCount.value stageCount))
            //            |> Seq.toList

            let perms = List.init 
                            (StageCount.value stageCount)
                            (fun _ -> TwoCyclePerm.makeRandomFullTwoCycle degree iRando)
                        |> List.map (TwoCyclePerm.toPermutation)
            let perms2 = List.init 
                            (StageCount.value stageCount)
                            (fun _ -> sTree.makePerm 0.29 iRando 4)
                         |> Result.sequence |> Result.ExtractOrThrow
                         |> List.map (TwoCyclePerm.toPermutation)
                         
            result {
                let! stp = perms2 |> TwoCycleGen.make3EightBlocks
                let atp = stp |> Seq.toArray
                return SorterGen.fromTwoCycleArray atp
            }


        let sorterArray = Array.init 
                                (SorterCount.value sorterCount)
                                (fun _ -> makeCoConjSorter () |> Result.ExtractOrThrow)  
    
        let coConjSorterSet = 
                    SorterSet.fromSorters 
                            coConjSorterSetId
                            degree 
                            sorterArray


        let sortableSetEx = SortableSet.Generated 
                                (SortableSetGenerated.allIntBits degree)
                                |> SortableSet.getSortableSetExplicit
                                |> Result.ExtractOrThrow 


        let perfBins = SortingOps.SorterSet.getSorterPerfBins
                          coConjSorterSet
                          sortableSetEx
                          Sorting.SwitchUsePlan.All
                          (UseParallel.create true)

        let pbr  = perfBins |> Result.ExtractOrThrow
        let rep = (pbr |> SorterPerfBin.binReport)
        Console.WriteLine rep
                  
        let ct =  pbr |> Array.sumBy(snd)

        Assert.IsTrue(true)



    [<TestMethod>]
    member this.checkSorterMs() =
        let rolloutOfAllBin16 = SortableSetRollout.allBinary
                                        (Degree.fromInt 16) 
                                 |> Result.ExtractOrThrow

        let sM = RefSorter.createRefSorter RefSorter.End16m
                 |> Result.ExtractOrThrow

        let resEndM = 
            SortingOps.evalGroupBySwitch
                sM
                rolloutOfAllBin16
                Sorting.SwitchUsePlan.All
            
        let switchUsesGrouping = 
                resEndM
                    |> SortingEval.SwitchEventRecords.getSwitchUses
                    |> Result.ExtractOrThrow
        
        
        Assert.IsTrue(switchUsesGrouping.weights.Length > 0)


    [<TestMethod>]
    member this.makePermSorter() =
        //let seed = 12345
        //let rngGen = (RngGen.createLcg seed)
        //let iRando = Rando.fromRngGen rngGen

        //let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        //let sorterSetId = SorterSetId.fromGuid (Guid.NewGuid())
        
        //let sorterLength =  degree |> SwitchOrStageCount.degreeTo999StageCount 
                                                    
        ////let sorterGen = SorterGen.RandStages (stageCount, rngGen, degree)
        //let sorterCount = SorterCount.fromInt 10
        //let makeSorter() = 
        //    SorterGen.createRandom degree sorterLength SwitchFrequency.max iRando

        //let sorterArray = Array.init 
        //                       (SorterCount.value sorterCount)
        //                       (fun _ -> makeSorter ())  

        //let sorterSet = 
        //            SorterSet.fromSorters 
        //                    sorterSetId
        //                    degree 
        //                    sorterArray

        //let sortableSetEx = SortableSet.Generated 
        //                        (SortableSetGenerated.allIntBits degree)
        //                        |> SortableSet.getSortableSetExplicit
        //                        |> Result.ExtractOrThrow 

        //let perfBins = SortingOps.SorterSet.getSorterPerfBins
        //                    sorterSet
        //                    sortableSetEx
        //                    Sorting.SwitchUsePlan.All
        //                    (UseParallel.create true)

        //let pbr  = perfBins |> Result.ExtractOrThrow
        //let rep = (pbr |> SorterPerfBin.binReport)
        //Console.WriteLine rep

        Assert.IsTrue(true)



    [<TestMethod>]
    member this.makeTreeSorter() =
        let seed = 12345
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
            SorterGen.fromTwoCycleArray perms2

        let sorterArray = Array.init 
                               (SorterCount.value sorterCount)
                               (fun _ -> makeSorter ())  

        let sorterSet = 
                    SorterSet.fromSorters 
                            sorterSetId
                            degree 
                            sorterArray

        let sortableSetEx = SortableSet.Generated 
                                (SortableSetGenerated.allIntBits degree)
                                |> SortableSet.getSortableSetExplicit
                                |> Result.ExtractOrThrow 

        let perfBins = SortingOps.SorterSet.getSorterPerfBins
                            sorterSet
                            sortableSetEx
                            Sorting.SwitchUsePlan.All
                            (UseParallel.create true)

        let pbr  = perfBins |> Result.ExtractOrThrow
        let rep = (pbr |> SorterPerfBin.binReport)
        let tot = pbr |> Array.sumBy(snd)
        Console.WriteLine (sprintf "tot:%d" tot)
        Console.WriteLine ""
        Console.WriteLine rep

        Assert.IsTrue(true)
