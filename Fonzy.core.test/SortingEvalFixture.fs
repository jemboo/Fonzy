namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SortingEvalFixture () =

    [<TestMethod>]
    member this.SorterPerfBin_fromSorterCoverage() =
        let yak = TestData.SortingEvalT.SorterCoverages()
        let bins = yak |> SortingEval.SorterPerfBin.fromSorterCoverages
        Assert.AreEqual(bins.Length, 2)


    [<TestMethod>]
    member this.fromSorterPerf () =
        let degree = Degree.fromInt 32
        let spfA = 
            {
                SortingEval.sorterPerf.usedStageCount = StageCount.fromInt 15;
                SortingEval.sorterPerf.usedSwitchCount = SwitchCount.fromInt 190
                SortingEval.sorterPerf.failCount = 0 |> SortableCount.fromInt |> Some
            }

        let spfB = 
            {
                SortingEval.sorterPerf.usedStageCount = StageCount.fromInt 15;
                SortingEval.sorterPerf.usedSwitchCount = SwitchCount.fromInt 188
                SortingEval.sorterPerf.failCount = 0 |> SortableCount.fromInt |> Some
            }

        let spfC = 
            {
                SortingEval.sorterPerf.usedStageCount = StageCount.fromInt 14;
                SortingEval.sorterPerf.usedSwitchCount = SwitchCount.fromInt 190
                SortingEval.sorterPerf.failCount = 0 |> SortableCount.fromInt |> Some
            }


        let sfA = SorterFitness.fromSorterPerf degree  (StageWeight.fromFloat 1.0) spfA
                  |> Energy.value
        let sfB = SorterFitness.fromSorterPerf degree  (StageWeight.fromFloat 1.0) spfB
                  |> Energy.value
        let sfC = SorterFitness.fromSorterPerf degree  (StageWeight.fromFloat 1.0) spfC
                  |> Energy.value


        Assert.IsTrue(sfA > sfB);
        Assert.IsTrue(sfB > sfC);




    [<TestMethod>]
    member this.SorterPerfBin_merge() =
        let binA1 = 
          { 
              SortingEval.sorterPerfBin.usedSwitchCount = (SwitchCount.fromInt 1);
              SortingEval.sorterPerfBin.usedStageCount = (StageCount.fromInt 1);
              SortingEval.sorterPerfBin.sorterCount = (SorterCount.fromInt 1);
              SortingEval.sorterPerfBin.successCount = 1;
              SortingEval.sorterPerfBin.failCount = 0;
          }
        let binA2 = 
          { 
              SortingEval.sorterPerfBin.usedSwitchCount = (SwitchCount.fromInt 1);
              SortingEval.sorterPerfBin.usedStageCount = (StageCount.fromInt 1);
              SortingEval.sorterPerfBin.sorterCount = (SorterCount.fromInt 1);
              SortingEval.sorterPerfBin.successCount = 1;
              SortingEval.sorterPerfBin.failCount = 0;
          }
        let binB = 
          { 
              SortingEval.sorterPerfBin.usedSwitchCount = (SwitchCount.fromInt 2);
              SortingEval.sorterPerfBin.usedStageCount = (StageCount.fromInt 1);
              SortingEval.sorterPerfBin.sorterCount = (SorterCount.fromInt 1);
              SortingEval.sorterPerfBin.successCount = 1;
              SortingEval.sorterPerfBin.failCount = 0;
          }
        let binC = 
          { 
              SortingEval.sorterPerfBin.usedSwitchCount = (SwitchCount.fromInt 1);
              SortingEval.sorterPerfBin.usedStageCount = (StageCount.fromInt 2);
              SortingEval.sorterPerfBin.sorterCount = (SorterCount.fromInt 1);
              SortingEval.sorterPerfBin.successCount = 1;
              SortingEval.sorterPerfBin.failCount = 0;
          }

        let testBins = seq { binA1; binA2; binB; binC }

        let merged = SortingEval.SorterPerfBin.merge testBins
                     |> Seq.toArray

        Assert.AreEqual(merged.Length, 2)