namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SortingGaTypesFixture () =

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


        let sfA = Fitness.fromSorterPerf spfA degree
                  |> Fitness.value
        let sfB = Fitness.fromSorterPerf spfB degree
                  |> Fitness.value
        let sfC = Fitness.fromSorterPerf spfC degree
                  |> Fitness.value

        Assert.IsTrue(sfA < sfB);
        Assert.IsTrue(sfB < sfC);
