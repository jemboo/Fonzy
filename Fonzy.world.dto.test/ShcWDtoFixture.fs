namespace Fonzy.world.dto.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ShcDtoFixture () =

    let degree = Degree.fromInt 12

    [<TestMethod>]
    member this.sorterShcResultDto() =
        Assert.AreEqual(1, 1);




    [<TestMethod>]
    member this.yabba2() =
        Assert.AreEqual(1, 1);

