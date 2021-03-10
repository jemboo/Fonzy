namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SortablesFixture () =

    [<TestMethod>]
    member this.SortableSetRollout_IsSorted() =
      let unSortedRollout = TestData.SorterParts.RolloutOfAllBinary
                            |> Result.ExtractOrThrow
      Assert.IsFalse(SortableSetRollout.isSorted unSortedRollout)

      let sortedRollout = TestData.SorterParts.RolloutOfAllSortedBinary
                              |> Result.ExtractOrThrow

      let a = unSortedRollout |> SortableSetRollout.toSortableIntArrays
                             |> Seq.countBy id
                             |> Seq.toArray

      Assert.IsTrue(a.Length > 0)
      Assert.IsTrue(SortableSetRollout.isSorted sortedRollout)


    [<TestMethod>]
    member this.SortableSetRollout_distinctResults() =
      let unsortedRollout = TestData.SorterParts.RolloutOfAllSortedBinary
                            |> Result.ExtractOrThrow
      let drs = unsortedRollout |> SortableSetRollout.distinctSortableSets
      Assert.IsTrue(drs.Length = 9)