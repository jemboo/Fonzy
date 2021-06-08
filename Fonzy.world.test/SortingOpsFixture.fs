namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SortingEval

[<TestClass>]
type SortingOpsFixture () =

    [<TestMethod>]
    member this.SorterSetCoverageCompBySAG() =
        let sorterSet = TestData.SorterSet.mediocreSorterSet
        let sortableSetInts = SortableSetSpec.Generated 
                                 (SortableSetGenerated.allIntBits sorterSet.degree)
                                 |> SortableSetSpec.getSortableSetExplicit
                                 |> Result.ExtractOrThrow 
        let ssInts = SortingOps.SorterSet.eval
                        sorterSet 
                        sortableSetInts 
                        Sorting.SwitchUsePlan.All
                        Sorting.EventGrouping.BySwitch
                        (UseParallel.create true)
                        (SortingEval.SortingRecords.getSorterCoverage true)
                        |> Result.ExtractOrThrow

        let sortableSetBp64 = SortableSetSpec.Generated 
                                   (SortableSetGenerated.allBp64 sorterSet.degree)
                                   |> SortableSetSpec.getSortableSetExplicit
                                   |> Result.ExtractOrThrow 

        let ssBp64 = SortingOps.SorterSet.eval
                        sorterSet 
                        sortableSetBp64 
                        Sorting.SwitchUsePlan.All
                        Sorting.EventGrouping.BySwitch
                        (UseParallel.create true)
                        (SortingEval.SortingRecords.getSorterCoverage true)
                        |> Result.ExtractOrThrow

        Assert.AreEqual(ssInts, ssBp64)


    [<TestMethod>]
    member this.SorterSetCoverageCompByNoSAG() =

        let sorterSet = TestData.SorterSet.mediocreSorterSet
        let sortableSetInts = SortableSetSpec.Generated 
                                    (SortableSetGenerated.allIntBits sorterSet.degree)
                                    |> SortableSetSpec.getSortableSetExplicit
                                    |> Result.ExtractOrThrow 
        let ssInts = SortingOps.SorterSet.eval
                        sorterSet 
                        sortableSetInts 
                        Sorting.SwitchUsePlan.All
                        Sorting.EventGrouping.NoGrouping
                        (UseParallel.create true)
                        (SortingEval.SortingRecords.getSorterCoverage true)
                        |> Result.ExtractOrThrow

        let sortableSetBp64 = SortableSetSpec.Generated 
                                    (SortableSetGenerated.allBp64 sorterSet.degree)
                                    |> SortableSetSpec.getSortableSetExplicit
                                    |> Result.ExtractOrThrow 

        let ssBp64 = SortingOps.SorterSet.eval
                        sorterSet 
                        sortableSetBp64 
                        Sorting.SwitchUsePlan.All
                        Sorting.EventGrouping.NoGrouping
                        (UseParallel.create true)
                        (SortingEval.SortingRecords.getSorterCoverage true)
                        |> Result.ExtractOrThrow

        Assert.AreEqual(ssInts, ssBp64)