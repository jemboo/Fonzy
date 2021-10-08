namespace Fonzy.core.dto.test
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SorterSetDtoFixture () =

    [<TestMethod>]
    member this.SorterSetDto() =
        let sorterSetCereal = TestData.SorterSet.mediocreSorterSet |> SorterSetDto.toJson
        let sorterSetBack = sorterSetCereal |> SorterSetDto.fromJson
                                            |> Result.ExtractOrThrow
        Assert.AreEqual(TestData.SorterSet.mediocreSorterSet, sorterSetBack);



    [<TestMethod>]
    member this.yabba2() =

        Assert.AreEqual(1, 1);