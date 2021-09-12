namespace Fonzy.world.dto.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ShcDtoFixture () =

    [<TestMethod>]
    member this.SorterSetDto() =
        let sorterSetCereal = TestData.SorterSet.mediocreSorterSet |> SorterSetDto.toJson
        let sorterSetBack = sorterSetCereal |> SorterSetDto.fromJson
                                            |> Result.ExtractOrThrow
        Assert.AreEqual(TestData.SorterSet.mediocreSorterSet, sorterSetBack);


    [<TestMethod>]
    member this.ShcStageWeightSpecDto () =
        let sw = StageWeight.fromFloat 2.2
        let sws = shcStageWeightSpec.Constant sw
        let swsDto = sws |> ShcStageWeightSpecDto.toDto
        let swsBack = swsDto |> ShcStageWeightSpecDto.fromDto
                             |> Result.ExtractOrThrow
        Assert.AreEqual(sws, swsBack);


    [<TestMethod>]
    member this.SorterMutSpecDto () =
        let swPfx = SwitchCount.fromInt 12
        let mutRt = MutationRate.fromFloat 0.1
        let smt = sorterMutationType.ByStage (swPfx, mutRt)
        let sms = sorterMutSpec.Constant smt
        let smsDto = sms |> SorterMutSpecDto.toDto
        let smsBack = smsDto |> SorterMutSpecDto.fromDto
                             |> Result.ExtractOrThrow
        Assert.AreEqual(sms, smsBack);


    [<TestMethod>]
    member this.sorterEvalSpecDto () =
        let sevSp = sorterEvalSpec.PerfBin
        let sevSpDto = sevSp |> SorterEvalSpecDto.toDto
        let sevSpBack = sevSpDto |> SorterEvalSpecDto.fromDto
                                 |> Result.ExtractOrThrow
        Assert.AreEqual(sevSp, sevSpBack);


    [<TestMethod>]
    member this.shcSaveDetailsDto () =
        let e = Energy.fromFloat 0.5
        let ssD = shcSaveDetails.EnergyThresh e
        let ssDdto = ssD |> ShcSaveDetailsDto.toDto
        let ssDback = ssDdto |> ShcSaveDetailsDto.fromDto
                             |> Result.ExtractOrThrow
        Assert.AreEqual(ssD, ssDback);



    [<TestMethod>]
    member this.sorterShcArchDto () =
        let degree = Degree.fromInt 12
        let stp = StepNumber.fromInt 5
        let rng = RngGen.createLcg (RandomSeed.fromInt 123)
        let sorter = RefSorter.goodRefSorterForDegree degree
                        |> Result.ExtractOrThrow
        let switchUses = SwitchUses.createOnes sorter.switchCount
        let wPerf = SwitchCount.fromInt 77
        let tPerf = StageCount.fromInt 17
        let sorterPerf = 
             { 
                 SortingEval.sorterPerf.usedSwitchCount = wPerf;
                 SortingEval.sorterPerf.usedStageCount = tPerf;
                 SortingEval.sorterPerf.successful = Some true;
             }

        let energy = Energy.fromFloat 1.0
        let archie = {
            sorterShcArch.step = stp;
            rngGen = Some rng;
            sorter = Some sorter;
            switchUses = Some switchUses;
            perf = sorterPerf;
            energy = energy;
          }

        let archieDto = archie |> SorterShcArchDto.toDto
        let archieBack = archieDto |> SorterShcArchDto.fromDto
                                   |> Result.ExtractOrThrow

        Assert.AreEqual(archie, archieBack);



    [<TestMethod>]
    member this.sorterShcSpecDto () =
        let degree = Degree.fromInt 12
        let stp = StepNumber.fromInt 5
        let rng = RngGen.createLcg (RandomSeed.fromInt 123)
        let sorter = RefSorter.goodRefSorterForDegree degree
                        |> Result.ExtractOrThrow
        let wPfx = [|1;2;3;0;|] |> Switch.fromIntArray
                   |> Seq.toArray
        let pfxSc = SwitchCount.fromInt wPfx.Length
        let mutRate = MutationRate.fromFloat 1.0
        let mutSpec = (pfxSc, mutRate) |> sorterMutationType.ByStage
                        |> sorterMutSpec.Constant 
                            
        //let sscSpec =
        //  {
        //    sorterShcSpec.rngGen = rng;
        //    sorter = sorter;
        //    switchPfx = wPfx;
        //    mutator = mutSpec;
        //    shcSortableSetSpec = ssRs;
        //    shcStageWeightSpec = swS;
        //    evaluator = evl;
        //    annealer = ann;
        //    updater = updt;
        //    terminator = term;
        //  }
        Assert.AreEqual(1, 1);