namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SortablesFixture () =

    [<TestMethod>]
    member this.SortableSet_getId() =
      let ss = TestData.SortableSet.sortableSet
      let ssId = ss |> SortableSet.getId

      Assert.AreEqual(ssId, TestData.SortableSet.sortableSetId)


    [<TestMethod>]
    member this.terst() =

      Assert.IsTrue(true)