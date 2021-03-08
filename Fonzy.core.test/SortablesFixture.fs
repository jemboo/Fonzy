namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SortablesFixture () =

    [<TestMethod>]
    member this.SortableSetRollup_IsSorted() =
      let unsortedRollup = TestData.SorterParts.rollupOfAllBinary
                            |> Result.ExtractOrThrow
      Assert.IsFalse(SortableSetRollup.isSorted unsortedRollup)

      let sortedRollup = TestData.SorterParts.rollupOfAllSortedBinary
                              |> Result.ExtractOrThrow
      Assert.IsTrue(SortableSetRollup.isSorted sortedRollup)