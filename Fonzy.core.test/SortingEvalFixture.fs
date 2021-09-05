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
                SortingEval.sorterPerf.successful = Some true;
                SortingEval.sorterPerf.usedStageCount = StageCount.fromInt 15;
                SortingEval.sorterPerf.usedSwitchCount = SwitchCount.fromInt 190
            }

        let spfB = 
            {
                SortingEval.sorterPerf.successful = Some true;
                SortingEval.sorterPerf.usedStageCount = StageCount.fromInt 15;
                SortingEval.sorterPerf.usedSwitchCount = SwitchCount.fromInt 188
            }

        let spfC = 
            {
                SortingEval.sorterPerf.successful = Some true;
                SortingEval.sorterPerf.usedStageCount = StageCount.fromInt 14;
                SortingEval.sorterPerf.usedSwitchCount = SwitchCount.fromInt 190
            }


        let sfA = SorterFitness.fromSorterPerf degree  (StageWeight.fromFloat 1.0) spfA
                  |> Energy.value
        let sfB = SorterFitness.fromSorterPerf degree  (StageWeight.fromFloat 1.0) spfB
                  |> Energy.value
        let sfC = SorterFitness.fromSorterPerf degree  (StageWeight.fromFloat 1.0) spfC
                  |> Energy.value


        Assert.IsTrue(sfA < sfB);
        Assert.IsTrue(sfB < sfC);




    [<TestMethod>]
    member this.test() =

      Assert.IsTrue(true)