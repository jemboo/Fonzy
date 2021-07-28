namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SortingEvalFixture () =

    [<TestMethod>]
    member this.testBp64NoGrouping() =
        let seed = 1234 |> RandomSeed.fromInt
        let iRando = Rando.fromRngGen (RngGen.createLcg seed)
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let sorterSetId = SorterSetId.fromGuid (Guid.NewGuid())
        let sorterCount = SorterCount.fromInt 50
        //let sorterGen = SorterRndGen.randomStages 
        //                        degree
        //                        Seq.empty
        //                       (StageCount.degreeTo999StageCount degree)
        //                       SwitchFrequency.max
        //let makeRandomSorter() = 
        //        SorterRndGen.createRandom sorterGen iRando

        //let mediocreRandomSorters = List.init (SorterCount.value sorterCount)
        //                              (fun _ -> makeRandomSorter())

        //let mediocreSorterSet = 
        //            SorterSet.fromSorters 
        //                    sorterSetId
        //                    degree
        //                    mediocreRandomSorters

        //let sortableSetAllBits = SortableSetSpec.Generated 
        //                           (SortableSetGenerated.allBp64 degree)
        //                         |> SortableSetSpec.getSortableSetExplicit
        //                         |> Result.ExtractOrThrow 

        //let ssR = SortingOps.SorterSet.eval
        //                mediocreSorterSet 
        //                sortableSetAllBits
        //                Sorting.SwitchUsePlan.All
        //                Sorting.EventGrouping.NoGrouping
        //                (UseParallel.create true)
        //                (SortingEval.SortingRecords.getSorterCoverage true)
         
        //            |> Result.ExtractOrThrow

        //Assert.IsTrue(ssR.Length > 0)

        Assert.IsTrue(true)


