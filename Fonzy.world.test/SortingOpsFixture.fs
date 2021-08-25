namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SortingEval

[<TestClass>]
type SortingOpsFixture () =

    [<TestMethod>]
    member this.SorterSetCoverageCompBySAG() =
        let sorterSet = TestData.SorterSet.mediocreSorterSet
        let sortableSetInts = sortableSetSpec.Generated 
                                 (SortableSetGen.allIntBits sorterSet.degree)
                                 |> SortableSetSpec.getSortableSet
                                 |> Result.ExtractOrThrow 
        let ssInts = SortingOps.SorterSet.eval
                        sorterSet 
                        sortableSetInts 
                        Sorting.switchUsePlan.All
                        Sorting.eventGrouping.BySwitch
                        (UseParallel.create true)
                        (SortingEval.SorterCoverage.fromSwitchEventRecords true)
                        |> Result.ExtractOrThrow

        let sortableSetBp64 = sortableSetSpec.Generated 
                                   (SortableSetGen.allBp64 sorterSet.degree)
                                   |> SortableSetSpec.getSortableSet
                                   |> Result.ExtractOrThrow 

        let ssBp64 = SortingOps.SorterSet.eval
                        sorterSet 
                        sortableSetBp64 
                        Sorting.switchUsePlan.All
                        Sorting.eventGrouping.BySwitch
                        (UseParallel.create true)
                        (SortingEval.SorterCoverage.fromSwitchEventRecords true)
                        |> Result.ExtractOrThrow

        Assert.AreEqual(ssInts, ssBp64)


    [<TestMethod>]
    member this.SorterSetCoverageCompByNoSAG() =

        let sorterSet = TestData.SorterSet.mediocreSorterSet
        let sortableSetInts = sortableSetSpec.Generated 
                                    (SortableSetGen.allIntBits sorterSet.degree)
                                    |> SortableSetSpec.getSortableSet
                                    |> Result.ExtractOrThrow 
        let ssInts = SortingOps.SorterSet.eval
                        sorterSet 
                        sortableSetInts 
                        Sorting.switchUsePlan.All
                        Sorting.eventGrouping.NoGrouping
                        (UseParallel.create true)
                        (SortingEval.SorterCoverage.fromSwitchEventRecords true)
                        |> Result.ExtractOrThrow

        let sortableSetBp64 = sortableSetSpec.Generated 
                                    (SortableSetGen.allBp64 sorterSet.degree)
                                    |> SortableSetSpec.getSortableSet
                                    |> Result.ExtractOrThrow 

        let ssBp64 = SortingOps.SorterSet.eval
                        sorterSet 
                        sortableSetBp64 
                        Sorting.switchUsePlan.All
                        Sorting.eventGrouping.NoGrouping
                        (UseParallel.create true)
                        (SortingEval.SorterCoverage.fromSwitchEventRecords true)
                        |> Result.ExtractOrThrow

        Assert.AreEqual(ssInts.Length, ssBp64.Length)


    [<TestMethod>]
    member this.removeDupesIntBitsByStage() =
  
      let unSortedIntSetRollout = 
            TestData.SorterActionRecords.intSetsRolloutOfAll

      let tss = TestData.SorterParts.twoStageSorter
      let fss = TestData.SorterParts.fourStageSorter
      let sss = TestData.SorterParts.sorterSegment

      let serTwoStage = 
          SortingInts.evalSorterOnIntSetsRollout
                            tss
                            unSortedIntSetRollout
                            Sorting.switchUsePlan.All
                            Sorting.eventGrouping.BySwitch

      let serFourStage = 
        SortingInts.evalSorterOnIntSetsRollout
                        fss
                        unSortedIntSetRollout
                        Sorting.switchUsePlan.All
                        Sorting.eventGrouping.BySwitch

      let rolloutFromTwoStage =
                    serTwoStage 
                        |> SwitchEventRecords.getSortableSetRollout
                        |> SortableSetRollout.removeDupes
                        |> Seq.toArray
                        |> IntSetsRollout.fromIntBits (TestData.degree)
                        |> Result.ExtractOrThrow

      let serTwoStep = 
          SortingInts.evalSorterOnIntSetsRollout
                            sss
                            rolloutFromTwoStage
                            Sorting.switchUsePlan.All
                            Sorting.eventGrouping.BySwitch

      let intBitsFourStage =  
            serFourStage 
                    |> SwitchEventRecords.getSortableSetRollout
                    |> SortableSetRollout.removeDupes
                    |> Seq.toList

      let intBitsTwoStep =  
            serTwoStep
                    |> SwitchEventRecords.getSortableSetRollout
                    |> SortableSetRollout.removeDupes
                    |> Seq.toList


      Assert.AreEqual(intBitsFourStage, intBitsTwoStep)


     [<TestMethod>]
     member this.removeDupesIntBitsBySwitch() =
        
     let unSortedIntSetRollout = 
            TestData.SorterActionRecords.intSetsRolloutOfAll
      
     let tss = TestData.SorterParts.goodRefSorter
      
     let serTwoStage = 
         SortingInts.evalSorterOnIntSetsRollout
                            tss
                            unSortedIntSetRollout
                            (Sorting.switchUsePlan.Range (0,1))
                            Sorting.eventGrouping.BySwitch
      
     let serFourStage = 
         SortingInts.evalSorterOnIntSetsRollout
                        tss
                        unSortedIntSetRollout
                        (Sorting.switchUsePlan.Range (0,2))
                        Sorting.eventGrouping.BySwitch
      
     let rolloutFromTwoStage =
                    serTwoStage 
                        |> SwitchEventRecords.getSortableSetRollout
                        |> SortableSetRollout.removeDupes
                        |> Seq.toArray
                        |> IntSetsRollout.fromIntBits (TestData.degree)
                        |> Result.ExtractOrThrow
      
     let serTwoStep = 
        SortingInts.evalSorterOnIntSetsRollout
                            tss
                            rolloutFromTwoStage
                            (Sorting.switchUsePlan.Range (1,2))
                            Sorting.eventGrouping.BySwitch
      
     let intBitsFourStage =  
             serFourStage 
                    |> SwitchEventRecords.getSortableSetRollout
                    |> SortableSetRollout.removeDupes
                    |> Seq.toList
      
     let intBitsTwoStep =  
            serTwoStep
                    |> SwitchEventRecords.getSortableSetRollout
                    |> SortableSetRollout.removeDupes
                    |> Seq.toList
      
    Assert.AreEqual(intBitsFourStage, intBitsTwoStep)


     [<TestMethod>]
     member this.removeDupesB64BySwitch() =
  
         let unSortedBP64SetRollout = 
          TestData.SorterActionRecords.bP64SetsRolloutOfAll

         let tss = TestData.SorterParts.goodRefSorter

         let serTwoStage = 
            SortingBp64.evalSorterOnBP64SetsRollout
                          tss
                          unSortedBP64SetRollout
                          (Sorting.switchUsePlan.Range (0,1))
                          Sorting.eventGrouping.BySwitch

         let serFourStage = 
             SortingBp64.evalSorterOnBP64SetsRollout
                      tss
                      unSortedBP64SetRollout
                      (Sorting.switchUsePlan.Range (0,2))
                      Sorting.eventGrouping.BySwitch

         let rolloutFromTwoStage =
                  serTwoStage 
                      |> SwitchEventRecords.getSortableSetRollout
                      |> SortableSetRollout.removeDupes
                      |> Seq.toArray
                      |> BP64SetsRollout.fromIntBits (TestData.degree)
                      |> Result.ExtractOrThrow

         let serTwoStep = 
             SortingBp64.evalSorterOnBP64SetsRollout
                          tss
                          rolloutFromTwoStage
                          (Sorting.switchUsePlan.Range (1,2))
                          Sorting.eventGrouping.BySwitch

         let intBitsFourStage =  
           serFourStage 
                  |> SwitchEventRecords.getSortableSetRollout
                  |> SortableSetRollout.removeDupes
                  |> Seq.toList

         let intBitsTwoStep =  
          serTwoStep
                  |> SwitchEventRecords.getSortableSetRollout
                  |> SortableSetRollout.removeDupes
                  |> Seq.toList

         Assert.AreEqual(intBitsFourStage, intBitsTwoStep)


    [<TestMethod>]
    member this.removeDupesBP64ByStage() =
  
      let unSortedBP64SetRollout = 
       TestData.SorterActionRecords.bP64SetsRolloutOfAll

      let tss = TestData.SorterParts.twoStageSorter
      let fss = TestData.SorterParts.fourStageSorter
      let sss = TestData.SorterParts.sorterSegment

      let serTwoStage = 
          SortingBp64.evalSorterOnBP64SetsRollout
                            tss
                            unSortedBP64SetRollout
                            Sorting.switchUsePlan.All
                            Sorting.eventGrouping.BySwitch

      let serFourStage = 
          SortingBp64.evalSorterOnBP64SetsRollout
                        fss
                        unSortedBP64SetRollout
                        Sorting.switchUsePlan.All
                        Sorting.eventGrouping.BySwitch

      let rolloutFromTwoStage =
                    serTwoStage 
                        |> SwitchEventRecords.getSortableSetRollout
                        |> SortableSetRollout.removeDupes
                        |> Seq.toArray
                        |> BP64SetsRollout.fromIntBits (TestData.degree)
                        |> Result.ExtractOrThrow

      let serTwoStep = 
          SortingBp64.evalSorterOnBP64SetsRollout
                            sss
                            rolloutFromTwoStage
                            Sorting.switchUsePlan.All
                            Sorting.eventGrouping.BySwitch

      let intBitsFourStage =  
            serFourStage 
                    |> SwitchEventRecords.getSortableSetRollout
                    |> SortableSetRollout.removeDupes
                    |> Seq.toList

      let intBitsTwoStep =  
            serTwoStep
                    |> SwitchEventRecords.getSortableSetRollout
                    |> SortableSetRollout.removeDupes
                    |> Seq.toList


      Assert.AreEqual(intBitsFourStage, intBitsTwoStep)



    [<TestMethod>]
    member this.oneStageReduceBp64() =
        let degree = Degree.fromInt 12
        let reducedBp64 = SortingOps.SortableSet.oneStageReduceBp64 degree
        let reducedIntB = SortingOps.SortableSet.oneStageReduceInts degree
        Assert.AreEqual(1,1)


