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
                sorterShcResult.id = ShcId.fromGuid (Guid.NewGuid())
                spec = TestData.SrtrShcSpec.sscSpec;
                msg = "OK"
                archives = [||]
            }
        let dto = sShcResult |> SorterShcResultDto.toDto
        let sShcResultBack = dto |> SorterShcResultDto.fromDto
                                 |> Result.ExtractOrThrow
        Assert.AreEqual(sShcResult, sShcResultBack);




    [<TestMethod>]
    member this.sorterShcResultsDto() =
        let sShcResult = 
            {
                sorterShcResult.id = ShcId.fromGuid (Guid.NewGuid())
                spec = TestData.SrtrShcSpec.sscSpec;
                msg = "OK"
                archives = [||]
            }
        let sShcResult2 = 
            {
                sorterShcResult.id = ShcId.fromGuid (Guid.NewGuid())
                spec = TestData.SrtrShcSpec.sscSpec;
                msg = "OK"
                archives = [||]
            }

        let ssRs = { sorterShcResults.members = [|sShcResult; sShcResult2|] }

        let dto = ssRs |> SorterShcResultsDto.toDto
        let ssRsBack = dto |> SorterShcResultsDto.fromDto
                           |> Result.ExtractOrThrow

        Assert.AreEqual(ssRs, ssRsBack);

