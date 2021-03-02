namespace Fonzy.core.dto.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type RootDtoFixture () =

    [<TestMethod>]
    member this.RootDtoForSorterDto() =
        let testDto = TestData.SorterParts.makeSorter() |> SorterDto.toDto
        let cereal =  RootDto.toJson testDto Map.empty
        let testDataBack = cereal |> RootDto.extractFromJson<SorterDto>
                                  |> Result.ExtractOrThrow
        let testDtoBack = fst testDataBack
        Assert.AreEqual(testDto, testDtoBack);




