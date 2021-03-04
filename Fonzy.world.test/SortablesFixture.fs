namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SortablesFixture () =

    [<TestMethod>]
    member this.rndBits() =
        let gu = TestData.SortableSet.sorterSetRndBits.id
        let gu2 = TestData.SortableSet.sorterSetRndBits2.id
        Assert.AreEqual(gu, gu2)


