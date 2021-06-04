namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SorterActionRecordsFixture() =

    [<TestMethod>]
    member this.SortableSetRollout_IsSorted() =
      let unSortedRollout = TestData.SorterActionRecords.intSetsRolloutAllBinary
      Assert.IsFalse(IntSetsRollout.isSorted unSortedRollout)
      let sortedRollout = TestData.SorterActionRecords.rolloutOfAllSortedBinary
      Assert.IsTrue(IntSetsRollout.isSorted sortedRollout)


    [<TestMethod>]
    member this.SortableSetRollout_distinctResults() =
      let unsortedRollout = TestData.SorterActionRecords.rolloutOfAllSortedBinary
      let drs = unsortedRollout |> IntSetsRollout.intBitsDistinct
      Assert.IsTrue(drs.Length = (Degree.value TestData.degree) + 1)


    [<TestMethod>]
    member this.SortableSetRollout_histogramOfSortableSets() =
      let unSortedRollout = TestData.SorterActionRecords.intSetsRolloutAllBinary
      let sortableCount = SortableCount.value unSortedRollout.sortableCount
      let histo = unSortedRollout |> IntSetsRollout.intBitsHist
      let totalCount = histo |> Array.sumBy(snd)
      Assert.AreEqual(sortableCount, totalCount)


    [<TestMethod>]
    member this.SwitchUseB32_toSwitchUses() =

      Assert.IsTrue(true)