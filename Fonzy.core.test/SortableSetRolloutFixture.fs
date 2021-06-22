namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SortableSetRolloutFixture() =

    [<TestMethod>]
    member this.SortableSetRollout_IsSorted() =
      let unSortedIntsRollout = TestData.SorterActionRecords.intSetsRolloutOfAll
                                |> sortableSetRollout.Int
      Assert.IsFalse(SortableSetRollout.isSorted unSortedIntsRollout)
      let sortedIntsRollout = TestData.SorterActionRecords.intSetsRolloutOfAllSorted
                                |> sortableSetRollout.Int
      Assert.IsTrue(SortableSetRollout.isSorted sortedIntsRollout)

      let unSortedBp64Rollout = TestData.SorterActionRecords.bP64SetsRolloutOfAll
                                |> sortableSetRollout.Bp64
      Assert.IsFalse(SortableSetRollout.isSorted unSortedBp64Rollout)
      let sortedBp64Rollout = TestData.SorterActionRecords.bp64SetsRolloutOfAllSorted
                                |> sortableSetRollout.Bp64
      Assert.IsTrue(SortableSetRollout.isSorted sortedBp64Rollout)



    [<TestMethod>]
    member this.IntSetsRollout_distinctResults() =
      let unsortedIntsRollout = TestData.SorterActionRecords.intSetsRolloutOfAllSorted
      let drs = unsortedIntsRollout |> IntSetsRollout.intBitsDistinct
      Assert.IsTrue(drs.Length = (Degree.value TestData.degree) + 1)



    [<TestMethod>]
    member this.SortableSetRollout_histogramOfSortableSets() =
      let unSortedRollout = TestData.SorterActionRecords.intSetsRolloutOfAll
      let sortableCount = SortableCount.value unSortedRollout.sortableCount
      let histo = unSortedRollout |> IntSetsRollout.intBitsHist
      let totalCount = histo |> Array.sumBy(snd)
      Assert.AreEqual(sortableCount, totalCount)


    [<TestMethod>]
    member this.removeDupesFromNoDupes() =
  
      let unSortedBp64Rollout = TestData.SorterActionRecords.bP64SetsRolloutOfAll
                                  |> sortableSetRollout.Bp64

      let intBitsFromBp =  unSortedBp64Rollout 
                            |> SortableSetRollout.toIntBits

      
      let intBitsBpUnique =  unSortedBp64Rollout 
                                |> SortableSetRollout.removeDupes
                                |> Seq.toArray

      Assert.AreEqual(intBitsFromBp.Length, intBitsBpUnique.Length)
      Assert.IsTrue(true)

