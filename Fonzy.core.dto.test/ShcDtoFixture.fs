namespace Fonzy.core.dto.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Newtonsoft.Json
open Microsoft.FSharpLu.Json
open System

[<TestClass>]
type ShcDtoFixture () =
    
    let degree = Degree.fromInt 12


    [<TestMethod>]
    member this.SerializeMap() =
        let kvps = [|("a","a"); ("b", "b"); ("c", "c"); ("d", "d") |]
                    |> Map.ofArray
        let json = JsonConvert.SerializeObject(kvps)
        let kvpsB = JsonConvert.DeserializeObject<Map<string,string>>(json)
        Assert.AreEqual(kvps, kvpsB)


    [<TestMethod>]
    member this.ShcStageWeightSpecLu () =
        let sw = StageWeight.fromFloat 2.2
        let sws = sorterStageWeightSpec.Constant sw
        let lulu = sw |> Compact.serialize
        let swsB = lulu |> Compact.deserialize

        let swsDto = sws |> ShcStageWeightSpecDto.toDto
        let swsBack = swsDto |> ShcStageWeightSpecDto.fromDto
                             |> Result.ExtractOrThrow
        Assert.AreEqual(1, 1);

    [<TestMethod>]
    member this.ShcStageWeightSpecDto () =
        let sw = StageWeight.fromFloat 2.2
        let sws = sorterStageWeightSpec.Constant sw
        let swsDto = sws |> ShcStageWeightSpecDto.toDto
        let swsBack = swsDto |> ShcStageWeightSpecDto.fromDto
                             |> Result.ExtractOrThrow
        Assert.AreEqual(sws, swsBack);


    [<TestMethod>]
    member this.SorterMutTypeDto () =
        let swPfx = SwitchCount.fromInt 12
        let mutRt = MutationRate.fromFloat 0.1
        let smt = sorterMutType.ByStage (swPfx, mutRt)
        let cereal = smt |> SorterMutTypeDto.toJson
        let smtBack = cereal |> SorterMutTypeDto.fromJson
                             |> Result.ExtractOrThrow
        Assert.AreEqual(smt, smtBack);


    [<TestMethod>]
    member this.SorterMutSpecDto () =
        let swPfx = SwitchCount.fromInt 12
        let mutRt = MutationRate.fromFloat 0.1
        let smt = sorterMutType.ByStage (swPfx, mutRt)
        let sms = sorterMutSpec.Constant smt
        let cereal = sms |> SorterMutSpecDto.toJson
        let smsBack = cereal |> SorterMutSpecDto.fromJson
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
    member this.sorterShcArchhDto () =
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
                 SortingEval.sorterPerf.failCount = 0 |> SortableCount.fromInt |> Some
             }
        let energy = Energy.fromFloat 1.0
        let fullArchie = {
                sorterShcArchFull.step = stp;
                advanceCount = 5;
                retreatCount = 1;
                rngGen = rng;
                sorter = sorter;
                switchUses = switchUses;
                perf = sorterPerf;
                energy = energy;
                }  
        let archie = fullArchie |> sorterShcArch.Full
        let archieDto = archie |> SorterShcArchDto.toDto
        let archieBack = archieDto |> SorterShcArchDto.fromDto
                                   |> Result.ExtractOrThrow

        Assert.AreEqual(archie, archieBack);


    [<TestMethod>]
    member this.sorterShcSpecDto () =
        let dto =  TestData.SrtrShcSpec.sscSpec |> SorterShcSpecDto.toDto
        let sscSpecBack = dto |> SorterShcSpecDto.fromDto
                              |> Result.ExtractOrThrow

        Assert.AreEqual(TestData.SrtrShcSpec.sscSpec, sscSpecBack);




    [<TestMethod>]
    member this.shcPivot () =
        let degree = Degree.fromInt 12
        let stageWeight = StageWeight.fromFloat 1.0
        //[SwitchCount, StageCount, SorterCount, SuccessCount, FailCount]
        let yab = 
            {
                sorterShcMergedDto.mergeCt = 1;
                sorterShcMergedDto.switchUseEntropy = 0.0;
                sorterShcMergedDto.switchAction = 0.0;
                sorterShcMergedDto.sorterId = "sorterId";
                sorterShcMergedDto.generation = 11;
                sorterShcMergedDto.mut = "mut"
                sorterShcMergedDto.temp = "temp"
                sorterShcMergedDto.degree = 111;
                sorterShcMergedDto.generationSpan = 1111;
                sorterShcMergedDto.perfBinsTrial = [|[|0;0;10;10;0|]; [|100;50;1;1;0|]; |]
                sorterShcMergedDto.perfBinsAccepted = [| [|0;0;10;10;0|]; [|100;50;1;1;0|]; |]
                sorterShcMergedDto.perfBinsCurrent = [|[|0;0;10;10;0|]; [|100;50;1;1;0|]; |]
            }

        let energyF = fun (pb:SortingEval.sorterPerfBin) ->  
            SorterFitness.weighted degree stageWeight pb.usedSwitchCount pb.usedStageCount
            |> Energy.value

        Console.WriteLine SorterShcMergedDto.pivotTableHdrs
        Console.WriteLine (SorterShcMergedDto.toReport energyF yab |> Result.ExtractOrThrow)

        Assert.AreEqual(1, 1);








