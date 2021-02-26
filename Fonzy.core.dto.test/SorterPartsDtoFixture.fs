namespace Fonzy.core.dto.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SorterPartsDtoFixture () =

    [<TestMethod>]
    member this.SwitchDto() =
        //let switch = Switch.
        Assert.AreEqual(1, 1);


    [<TestMethod>]
    member this.SorterDto() =
        let sorter = TestData.SorterParts.makeSorter()
        let sorterDto = sorter |> SorterDto.toDto
        let sorterBack = sorterDto |> SorterDto.fromDto
                                   |> Result.ExtractOrThrow
        Assert.AreEqual(sorter, sorterBack);
