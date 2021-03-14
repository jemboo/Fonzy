namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SorterActionRecordsFixture() =

    [<TestMethod>]
    member this.SortableSetRollout_IsSorted() =
      let unSortedRollout = TestData.SorterActionRecords.rolloutOfAllBinary
      Assert.IsFalse(SortableSetRollout.isSorted unSortedRollout)
      let sortedRollout = TestData.SorterActionRecords.rolloutOfAllSortedBinary
      Assert.IsTrue(SortableSetRollout.isSorted sortedRollout)


    [<TestMethod>]
    member this.SortableSetRollout_distinctResults() =
      let unsortedRollout = TestData.SorterActionRecords.rolloutOfAllSortedBinary
      let drs = unsortedRollout |> SortableSetRollout.distinctSortableSets
      Assert.IsTrue(drs.Length = (Degree.value TestData.degree) + 1)


    [<TestMethod>]
    member this.SortableSetRollout_histogramOfSortableSets() =
      let unSortedRollout = TestData.SorterActionRecords.rolloutOfAllBinary
      let sortableCount = SortableCount.value unSortedRollout.sortableCount
      let histo = unSortedRollout |> SortableSetRollout.histogramOfSortableSets
      let totalCount = histo |> Array.sumBy(snd)
      Assert.AreEqual(sortableCount, totalCount)