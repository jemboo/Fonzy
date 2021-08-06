namespace Fonzy.core.dto.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SorterGenDtoFixture () =

    [<TestMethod>]
    member this.SorterMutationTypeDto() =
        let switchCount = SwitchCount.fromInt 22
        let mutRate = MutationRate.fromFloat 0.55
        let smt = sorterMutationType.ByStage (switchCount, mutRate)
        let smtDto = SorterMutationTypeDto.toDto smt
        let smtBack = SorterMutationTypeDto.fromDto smtDto
                      |> Result.ExtractOrThrow
        Assert.AreEqual(smt, smtBack);


    //[<TestMethod>]
    //member this.SorterMutationSpecDto() =
    //    let switchCount = SwitchCount.fromInt 22
    //    let mutRate = MutationRate.fromFloat 0.55
    //    let smt = sorterMutationType.BySwitch (switchCount, mutRate)
    //    let seed = RandomSeed.fromInt 1234
    //    let rndGen = RngGen.createLcg seed
    //    let sms = sorterMutationSpec (smt, rndGen)
    //    let smsDto = sms |> SorterMutationSpecDto.toDto
    //    let smsBack = smsDto |> SorterMutationSpecDto.fromDto
    //                         |> Result.ExtractOrThrow
    //    Assert.AreEqual(sms, smsBack);


    [<TestMethod>]
    member this.yabba() =

        Assert.AreEqual(1, 1);