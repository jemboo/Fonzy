namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SortableGenFixture () =

    [<TestMethod>]
    member this.rndBits() =

        Assert.AreNotEqual(1, 2)


