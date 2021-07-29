namespace Fonzy.core.dto.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SorterPartsDtoFixture () =

    [<TestMethod>]
    member this.SwitchDto() =
        //let switch = Switch.
        let t = typeof<sorterDto>
        let s = t.ToString()
        Assert.AreEqual(1, 1);


    [<TestMethod>]
    member this.SorterDto() =
        let sorter = TestData.SorterParts.makeRandomSorter()
        let sorterDto = sorter |> SorterDto.toDto
        let sorterBack = sorterDto |> SorterDto.fromDto
                                   |> Result.ExtractOrThrow
        Assert.AreEqual(sorter, sorterBack);


    [<TestMethod>]
    member this.SorterSetDto() =
        let sorterSetCereal = TestData.SorterSet.mediocreSorterSet |> SorterSetDto.toJson
        let sorterSetBack = sorterSetCereal |> SorterSetDto.fromJson
                                            |> Result.ExtractOrThrow
        Assert.AreEqual(TestData.SorterSet.mediocreSorterSet, sorterSetBack);