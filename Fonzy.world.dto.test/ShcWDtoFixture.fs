namespace Fonzy.world.dto.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ShcWDtoFixture () =

    let degree = Degree.fromInt 12

    [<TestMethod>]
    member this.sorterShcResultDto() =
        let sShcResult = 
            {
                sorterShcResult.spec = TestData.SrtrShcSpec.sscSpec;
                sorterShcResult.msg = "OK"
                sorterShcResult.archives = [||]
            }
        let dto = sShcResult |> SorterShcResultDto.toDto
        let sShcResultBack = dto |> SorterShcResultDto.fromDto
                                 |> Result.ExtractOrThrow
        Assert.AreEqual(sShcResult, sShcResultBack);




    [<TestMethod>]
    member this.sorterShcResultsDto() =
        let sShcResult = 
            {
                sorterShcResult.spec = TestData.SrtrShcSpec.sscSpec;
                sorterShcResult.msg = "OK"
                sorterShcResult.archives = [||]
            }
        let sShcResult2 = 
            {
                sorterShcResult.spec = TestData.SrtrShcSpec.sscSpec;
                sorterShcResult.msg = "OK"
                sorterShcResult.archives = [||]
            }

        let ssRs = { sorterShcResults.members = [|sShcResult; sShcResult2|] }

        let dto = ssRs |> SorterShcResultsDto.toDto
        let ssRsBack = dto |> SorterShcResultsDto.fromDto
                           |> Result.ExtractOrThrow

        Assert.AreEqual(ssRs, ssRsBack);

