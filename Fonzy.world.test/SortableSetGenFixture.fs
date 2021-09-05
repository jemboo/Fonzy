namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SortingEval

[<TestClass>]
type SortableSetGenFixture () =

    [<TestMethod>]
    member this.SortableSetSpecReduced_make() =
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
    member this.SortableSetSpecReduced_makeMemoize() =
        let degree = Degree.fromInt 16
        let prefix1 = [|{Switch.hi=3; low=0;}|]
        let prefix2 = [|{Switch.hi=3; low=1;}|]
        let ssGen = SortableSetGen.allBp64 degree
                    |> sortableSetSpec.Generated

        let ssetT1a, wUsesT1a = 
            (ssGen, prefix1) |> sortableSetSpecReduced
                             |> SortableSetSpecReduced.makeMemoize
                             |> Result.ExtractOrThrow

        let ssetT1b, wUsesT1b = 
            (ssGen, prefix1) |> sortableSetSpecReduced
                             |> SortableSetSpecReduced.makeMemoize
                             |> Result.ExtractOrThrow

        let ssetT2, wUsesT2 = 
            (ssGen, prefix2) |> sortableSetSpecReduced
                             |> SortableSetSpecReduced.makeMemoize
                             |> Result.ExtractOrThrow


        Assert.IsTrue(true)


