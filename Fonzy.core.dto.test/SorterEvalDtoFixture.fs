namespace Fonzy.core.dto.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SorterEvalDtoFixture () =

    [<TestMethod>]
    member this.sorterPerfDto() =
        let w = SwitchCount.fromInt 22
        let t = StageCount.fromInt 12
        let perf = {SortingEval.sorterPerf.failCount = 0 |> SortableCount.fromInt |> Some
                    SortingEval.sorterPerf.usedStageCount = t;
                    SortingEval.sorterPerf.usedSwitchCount = w;}
        let perfDto = SorterPerfDto.toDto perf
        let perfBack = SorterPerfDto.fromDto perfDto
                       |> Result.ExtractOrThrow
        Assert.AreEqual(perf, perfBack);

        let perf2 = {SortingEval.sorterPerf.failCount = None;
                    SortingEval.sorterPerf.usedStageCount = t;
                    SortingEval.sorterPerf.usedSwitchCount = w;}
        let perfDto2 = SorterPerfDto.toDto perf2
        let perfBack2 = SorterPerfDto.fromDto perfDto2
                       |> Result.ExtractOrThrow
        Assert.AreEqual(perf2, perfBack2);



    [<TestMethod>]
    member this.sorterSavingDto() =
        let stageWeight = StageWeight.fromFloat 2.2
        let sorterCount = SorterCount.fromInt 5
        let ssv = sorterSaving.All // (stageWeight, sorterCount)
        let ssvDto = SorterSavingDto.toDto ssv
        let ssvBack = SorterSavingDto.fromDto ssvDto
                      |> Result.ExtractOrThrow
        Assert.AreEqual(ssv, ssvBack);

        let ssv2 = sorterSaving.Perf (stageWeight, sorterCount)
        let ssvDto2 = SorterSavingDto.toDto ssv2
        let ssvBack2 = SorterSavingDto.fromDto ssvDto2
                      |> Result.ExtractOrThrow
        Assert.AreEqual(ssv2, ssvBack2);
