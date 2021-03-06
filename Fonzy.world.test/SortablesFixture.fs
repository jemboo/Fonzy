namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SortablesFixture () =

    [<TestMethod>]
    member this.rndBits() =
        let gu = TestData.SortableSet.rndBits.id
        let gu2 = TestData.SortableSet.rndBits2.id
        Assert.AreNotEqual(gu, gu2)


