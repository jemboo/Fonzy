namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SortingEval

[<TestClass>]
type SortableSetGenFixture () =

    [<TestMethod>]
    member this.SortableSetSpecReduced() =
        let degree = Degree.fromInt 16
        let prefix = [|{Switch.hi=3; low=0;}|]
        let ssGen = SortableSetGen.allBp64 degree
                    |> sortableSetSpec.Generated
        let sset, wUses = 
            (ssGen, [||])   |> sortableSetSpecReduced
                            |> SortableSetSpecReduced.make
                            |> Result.ExtractOrThrow

        let ssetT, wUsesT = 
            (ssGen, prefix) |> sortableSetSpecReduced
                            |> SortableSetSpecReduced.make
                            |> Result.ExtractOrThrow

        let fullLen = sset |> SortableSet.toIntBits |> Seq.length 
        let trimLen = ssetT |> SortableSet.toIntBits |> Seq.length 

        Assert.IsTrue(fullLen > trimLen)


    [<TestMethod>]
    member this.Aab() =

        Assert.IsTrue(true)


