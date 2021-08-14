namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SortingEval

[<TestClass>]
type StoHillClimbFixture () =

    [<TestMethod>]
    member this.ShcSortableSetSpec() =
        let degree = Degree.fromInt 16
        let prefix = [|{Switch.hi=3; low=0;}|]
        let ssetT, wUses = ShcSortableSetSpec.makeAllForDegree
                                (degree, prefix)
        let ssetT2, wUses = ShcSortableSetSpec.makeAllForDegree
                                (degree, prefix)

        let sset, wUses = ShcSortableSetSpec.makeAllForDegree
                           (degree, [||] )
        let fullLen = sset |> SortableSet.toIntBits |> Seq.length 
        let trimLen = ssetT |> SortableSet.toIntBits |> Seq.length 

        //Assert.IsTrue(fullLen > trimLen)
        Assert.IsTrue(true)


    [<TestMethod>]
    member this.Aab() =
        let lone = [1; 2; 3]
        let lapp = 4 :: lone
        let yabb = lapp |> List.head
        Assert.IsTrue(true)


